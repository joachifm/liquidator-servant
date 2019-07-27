{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import GHC.Generics (Generic)

import qualified Control.Exception as E
import Control.Monad.Except (ExceptT(ExceptT))

import Data.Int (Int64)
import Data.Word (Word64)

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text.Encoding as Text

import Data.IORef (IORef, newIORef, atomicModifyIORef')
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Casing
import qualified Data.Aeson as Aeson

import Servant hiding (BasicAuth)

import Servant.Auth.Server
import Crypto.JOSE.JWK (JWK)

import qualified Network.Wai.Handler.Warp as Warp

import IdSupply
import Money
import UuidGen

------------------------------------------------------------------------------

-- |
-- $architecture
-- = General component architecture
--
-- All components follow the @Handle@ pattern.  A single 'Handle' carries all
-- state, and all operations pertaining to the outermost layer of the
-- application occur in 'IO'.
--
-- All business logic is implemented as pure functions.  An adapter layer
-- mediates data between the outer and the innermost layer, as necessary.
--
-- Conceptually, a component is
--
-- >> data Input   -- All inputs gathered from external sources
-- >> data Output  -- All outputs for external consumers
-- >> businessLogic :: Input -> Output
-- >> getInput :: IO Input
-- >> putOutput :: Output -> IO ()
-- >> component :: IO ()
-- >> component = getInput >>= pure . component >>= putOutput

------------------------------------------------------------------------------

-- |
-- $identdesign
-- = Internal vs external identifiers
--
-- Generic, internal identifiers are 64-bit numbers.  Ideally, these
-- identifiers should not be user visible.
--
-- For externally visible surrogate keys, we want monotonically increasing yet
-- hard-to-predict values.

------------------------------------------------------------------------------

-- |
-- $schemadesign
-- = JSON schema mappings
--
-- For each schema, we typically define two structures: one for the body
-- comprising the actual data and one that wraps the body.
--
-- As a form of namespacing, records are prefixed by all-lowercase abbreviated
-- version of the record type name.  When converting to/from JSON, the prefix
-- is stripped and everything after the prefix is converted to @snake_case@.

------------------------------------------------------------------------------

-- |
-- $authdesign
-- = Authorization
--
-- Two types of tokens
--
-- [@refresh@] Long-lived token, granted based on credentials; a refresh
-- token, in turn, may be used to acquire @access@ tokens.
--
-- [@access@]  A token that grants access to a protected resource.
--
-- The idea is that we only need to authenticate the user when the @refresh@
-- token is handed out and then use @access@ tokens to regulate access to
-- protected resources, not having to re-authenticate until the @refresh@
-- token is invalidated.

------------------------------------------------------------------------------

-- | The type of internal identifiers.
type GenericId = Int64

------------------------------------------------------------------------------

data Conf = Conf
  { cJwkFile :: !(Maybe FilePath)
  , cSslKeyFile :: !FilePath
  , cSslCertFile :: !FilePath
  }
  deriving (Generic, Show, FromJSON, ToJSON)

defaultConf :: Conf
defaultConf = Conf
  { cJwkFile = Nothing
  , cSslCertFile = "server.crt"
  , cSslKeyFile = "server.key"
  }

type Login = ByteString
type Password = ByteString

data Handle = Handle
  { hConf :: Conf
  , hJwtSettings :: !JWTSettings
  , hIdSupply :: IdSupply
  , hSecretStore :: Map GenericId Text
  , hAuthUsers :: Map (Login, Password) AuthUser
  }

nextId :: Handle -> IO GenericId
nextId =  supplyNextId . hIdSupply

loadOrGenerateJwk :: Maybe FilePath -> IO JWK
loadOrGenerateJwk (Just keyFile) = readKey keyFile
loadOrGenerateJwk Nothing        = generateKey

newHandle
  :: Conf
  -> IO Handle
newHandle cfg
  = Handle <$> pure cfg
           <*> (defaultJWTSettings <$> loadOrGenerateJwk (cJwkFile cfg))
           <*> newIdSupply
           <*> pure mempty
           <*> pure (Map.fromList [(("root", "hunter2"), AuthUser 0    AuthRoleAdmin)
                                  ,(("user", "hunter2"), AuthUser 1000 AuthRoleUser)
                                  ])

------------------------------------------------------------------------------

-- | A natural transformation from 'IO' to servant's 'Handler'.
convert
  :: IO a
  -> Handler a
convert
  = Handler . ExceptT . E.try

------------------------------------------------------------------------------

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

------------------------------------------------------------------------------

data AuthRole
  = AuthRoleOwner
  | AuthRoleUser
  | AuthRoleAdmin
  deriving (Generic, FromJSON, ToJSON)

data AuthUser = AuthUser
  { authUserId :: !Word64
  , authUserRole :: !AuthRole
  }
  deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

data ReqLoginBody = ReqLoginBody
  { rqloginbodyUserName :: Text
  , rqloginbodyPassword :: Text
  }
  deriving (Generic, Show)

-- @note copypaste, generate this?
instance ToJSON ReqLoginBody where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

-- @note copypaste, generate this?
instance FromJSON ReqLoginBody where
  parseJSON = Aeson.genericParseJSON aesonOptions

data ReqLogin = ReqLogin
  { rqloginUser :: ReqLoginBody
  }
  deriving (Generic, Show)

-- @note copypaste, generate this?
instance ToJSON ReqLogin where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

-- @note copypaste, generate this?
instance FromJSON ReqLogin where
  parseJSON = Aeson.genericParseJSON aesonOptions

------------------------------------------------------------------------------

basicAuthCheck
  :: Handle
  -> BasicAuthData
  -> IO (AuthResult AuthUser)
basicAuthCheck h (BasicAuthData nam pw)
  = pure $ maybe Indefinite Authenticated $ Map.lookup (nam, pw) (hAuthUsers h)

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthUser)

instance FromBasicAuthData AuthUser where
  fromBasicAuthData
    :: BasicAuthData
    -> (BasicAuthData -> IO (AuthResult AuthUser))
    -> IO (AuthResult AuthUser)
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

------------------------------------------------------------------------------

-- | Create a new refresh token.
type CreateRefreshToken
  =  ReqBody '[JSON] ReqLogin
  :> Post '[JSON] AuthUser

type TokenApi
  = "token" :> CreateRefreshToken

type AuthApi
  = TokenApi

------------------------------------------------------------------------------

type AuthList = '[ JWTSettings
                 , CookieSettings
                 , BasicAuthData -> IO (AuthResult AuthUser)
                 ]

authContextVal
  :: Handle
  -> Context AuthList
authContextVal h
  =  hJwtSettings h
  :. defaultCookieSettings
  :. basicAuthCheck h
  :. EmptyContext

authApi :: Proxy AuthApi
authApi = Proxy @AuthApi

authHandler
  :: Handle
  -> ServerT AuthApi IO
authHandler h
  =    createRefreshTokenHandler
  where
    createRefreshTokenHandler (ReqLogin body) = do
      print body
      return $! AuthUser 0 AuthRoleAdmin

authServer
  :: Handle
  -> Server AuthApi
authServer
  = hoistServer authApi convert . authHandler

authApp
  :: Handle
  -> Application
authApp
  = serve authApi . authServer

------------------------------------------------------------------------------

type ProtectedApi
  =  Auth '[ JWT, BasicAuth ] AuthUser
  :> ( "secret" :> Get '[JSON] NoContent )

protectedHandler
  :: Handle
  -> ServerT ProtectedApi IO
protectedHandler h (Authenticated authUser) = do
  return NoContent
protectedHandler _ _ = E.throwIO err401

------------------------------------------------------------------------------

type Api
  =    AuthApi
  :<|> ProtectedApi

api :: Proxy Api
api = Proxy

handler
  :: Handle
  -> ServerT Api IO
handler h
  =    authHandler h
  :<|> protectedHandler h

server
  :: Handle
  -> Server Api
server h
  = hoistServerWithContext api (Proxy @AuthList) convert (handler h)

app
  :: Handle
  -> Application
app h
  = serveWithContext api (authContextVal h) (server h)

------------------------------------------------------------------------------

run :: IO ()
run
  = Warp.runSettings warpSettings
  . app =<< newHandle defaultConf

warpSettings :: Warp.Settings
warpSettings
  = announce
  . devSettings
  $ Warp.defaultSettings

announce :: (Warp.Settings -> Warp.Settings)
announce = \ws -> flip Warp.setBeforeMainLoop ws $ putStrLn $
      "Listening on " <> show (Warp.getHost ws)
                      <> ":"
                      <> show (Warp.getPort ws)

devSettings :: (Warp.Settings -> Warp.Settings)
devSettings
  = Warp.setPort 3000
  . Warp.setHost "127.0.0.1"
  . Warp.setLogger stdLogger
  where
    stdLogger req status _ = print req >> print status >> putStrLn ""
