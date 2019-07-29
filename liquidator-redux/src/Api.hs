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

import Data.Text (Text)
import Data.Int (Int64)

import Data.Map (Map)
import qualified Data.Map.Lazy as Map

import Data.IORef

import Data.Aeson (FromJSON, ToJSON)

import Servant

import Lucid (Html)
import Lucid.Html5
import Servant.HTML.Lucid

import Servant.Auth.Server

import qualified Network.Wai.Handler.Warp as Warp

------------------------------------------------------------------------

convert
  :: IO a
  -> Handler a
convert
  = Handler . ExceptT . E.try

------------------------------------------------------------------------

type Username = Text

type Password = Text

data Handle = Handle
  { jwtSettings :: !JWTSettings
  , cookieSettings :: !CookieSettings
  , userLoginCreds :: !(IORef (Map (Username, Password) Int))
  , secret :: !(IORef Secret)
  }

newHandle :: IO Handle
newHandle
  = Handle <$> (defaultJWTSettings <$> generateKey)
           <*> pure defaultCookieSettings
           <*> newIORef mempty
           <*> newIORef (MkSecret "hush")

------------------------------------------------------------------------

data Session = Session
  { sessionId :: Int
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

------------------------------------------------------------------------

data Secret = MkSecret
  { secretText :: !Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

------------------------------------------------------------------------

type SecretApi
  =  Auth '[Cookie, JWT] Session
  :> "secret"
  :> (    Get '[JSON] Secret
     :<|> ReqBody '[JSON] Secret :> Post '[JSON] NoContent
     )

------------------------------------------------------------------------

secretHandler
  :: Handle
  -> ServerT SecretApi IO
secretHandler h (Authenticated sess)
  =    getSecretHandler
  :<|> createSecretHandler
  where
    getSecretHandler :: IO Secret
    getSecretHandler = readIORef (secret h)

    createSecretHandler
      :: Secret
      -> IO NoContent
    createSecretHandler newSecret = do
      writeIORef (secret h) newSecret
      return NoContent
secretHandler _ _ = E.throwIO err404 :<|> \_ -> E.throwIO err404

------------------------------------------------------------------------

type Api = SecretApi

handler :: Handle -> ServerT Api IO
handler = secretHandler

api :: Proxy Api
api = Proxy

server
  :: Handle
  -> Server Api
server = hoistServerWithContext api ctx convert . handler
  where
    ctx = Proxy @'[CookieSettings, JWTSettings]

mkApp
  :: Handle
  -> Application
mkApp h = serveWithContext api context (server h)
  where
    context
      = cookieSettings h
      :. jwtSettings h
      :. EmptyContext

app :: IO Application
app = mkApp <$> newHandle

------------------------------------------------------------------------

run :: IO ()
run
  = Warp.runSettings warpSettings =<< app

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
