{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import GHC.Generics (Generic)

import qualified Control.Exception as E
import Control.Monad.Except (ExceptT(ExceptT))

import Data.Word (Word64)
import Data.Text (Text)
import Data.Int (Int64)

import Data.IORef
import Data.Map (Map)
import qualified Data.Map.Lazy as Map

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Casing
import qualified Data.Aeson as Aeson

import Data.UUID (UUID)
import qualified Data.UUID.V1 as UUID

import Control.Concurrent (threadDelay)

import Servant
import Servant.Auth.Server

import qualified Network.Wai.Handler.Warp as Warp

------------------------------------------------------------------------------

-- |
-- = Internal vs external identifiers
--
-- Generic, internal identifiers are 64-bit numbers.  Ideally, these
-- identifiers should not be user visible.
--
-- For externally visible surrogate keys, we want monotonically increasing yet
-- hard-to-predict values.

-- | The type of internal identifiers.
type GenericId = Int64

------------------------------------------------------------------------

-- | Exhausted UUID supply.
data UiidGenException = MkUuidGenException
  deriving (Eq, Show)

instance E.Exception UiidGenException

-- | A variant of 'Data.UUID.V1.nextUUID' that retries generation in case of
-- failure, upto some maximum number of retries.
--
-- Throws 'UuidGenException' after exceeding maximum number of retries.
nextUUID :: IO UUID
nextUUID = loop 1000
  where
    loop :: Int -> IO UUID
    loop 0 = E.throwIO MkUuidGenException
    loop n = do
      m <- UUID.nextUUID
      case m of
        Just v  -> return v
        Nothing -> threadDelay 250 >> loop (n - 1)

------------------------------------------------------------------------------

type IdSupply = IORef GenericId

newIdSupply :: IO IdSupply
newIdSupply = newIORef 1

supplyNextId :: IdSupply -> IO GenericId
supplyNextId = flip atomicModifyIORef' (\i -> (i + 1, i))

------------------------------------------------------------------------------

data Conf = Conf
  { cJwtSettings :: !JWTSettings
  }

data Handle = Handle
  { hConf :: Conf
  , hIdSupply :: IdSupply
  }

nextId :: Handle -> IO GenericId
nextId =  supplyNextId . hIdSupply

newHandle
  :: Conf
  -> IO Handle
newHandle cfg
  = Handle <$> pure cfg
           <*> newIdSupply

------------------------------------------------------------------------------

-- |
-- = JSON schema mappings
--
-- For each schema, we typically define two structures: one for the body
-- comprising the actual data and one that wraps the body.
--
-- As a form of namespacing, records are prefixed by all-lowercase abbreviated
-- version of the record type name.  When converting to/from JSON, the prefix
-- is stripped and everything after the prefix is converted to @snake_case@.

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

------------------------------------------------------------------------------

newtype User = User { userId :: Word64 }
  deriving (Generic, FromJSON, ToJSON, FromJWT, ToJWT)

------------------------------------------------------------------------------

data RequestLoginBody = RequestLoginBody
  { reqloginbodyUserName :: Text
  , reqloginbodyPassword :: Text
  }
  deriving (Generic, Show)

-- @note copypaste, generate this?
instance ToJSON RequestLoginBody where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

-- @note copypaste, generate this?
instance FromJSON RequestLoginBody where
  parseJSON = Aeson.genericParseJSON aesonOptions

data RequestLogin = RequestLogin
  { reqloginUser :: RequestLoginBody
  }
  deriving (Generic)

-- @note copypaste, generate this?
instance ToJSON RequestLogin where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

-- @note copypaste, generate this?
instance FromJSON RequestLogin where
  parseJSON = Aeson.genericParseJSON aesonOptions

------------------------------------------------------------------------------

-- | Create a new refresh token.
type CreateRefreshToken
  =  ReqBody '[JSON] RequestLogin
  :> Post '[JSON] User

createRefreshToken
  :: Handle
  -> RequestLogin
  -> IO User
createRefreshToken = undefined

type TokenApi
  = "token" :> CreateRefreshToken

type AuthApi
  = TokenApi

------------------------------------------------------------------------------

convert
  :: IO a
  -> Handler a
convert
  = Handler . ExceptT . E.try

------------------------------------------------------------------------------

authApi :: Proxy AuthApi
authApi = Proxy @AuthApi

authHandler
  :: ServerT AuthApi IO
authHandler
  =    createRefreshTokenHandler
  where
    createRefreshTokenHandler (RequestLogin body) = do
      print body
      return $! User 0

authServer
  :: Server AuthApi
authServer
  = hoistServer authApi convert authHandler

authApp
  :: Application
authApp
  = serve authApi authServer

------------------------------------------------------------------------------

run :: IO ()
run
  = Warp.runSettings (devSettings Warp.defaultSettings) authApp

devSettings :: (Warp.Settings -> Warp.Settings)
devSettings
  = Warp.setPort 3000
  . Warp.setHost "127.0.0.1"
  . Warp.setLogger (\req status _ -> print req >> print status >> putStrLn "")

------------------------------------------------------------------------------
