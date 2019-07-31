{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator where

import GHC.Generics (Generic)

import Control.Monad (when)
import Control.Monad.Except (ExceptT(ExceptT))
import qualified Control.Exception as E

import Control.Lens.Operators
import Control.Lens.TH

import Data.Int (Int32, Int64)
import Data.Word (Word32)

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Data.Map (Map)
import qualified Data.Map.Lazy as Map

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Aeson as Aeson

import Web.FormUrlEncoded

import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  )

import Data.Time.Clock.System
  ( SystemTime(..)
  , getSystemTime
  )

import Servant

import Lucid (Html, toHtml)
import Lucid.Html5
import Servant.HTML.Lucid

import Servant.Auth.Server
import Crypto.JOSE.JWK (JWK)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import Network.Wai (Application, Middleware)
import qualified Network.Wai.Middleware.AddHeaders as AddHeaders
import qualified Network.Wai.Middleware.ForceSSL as ForceSSL
import qualified Network.Wai.Middleware.Gzip as Gzip

import Options.Applicative

import IORef
import Money

------------------------------------------------------------------------

type GenericId = Int64

------------------------------------------------------------------------

data Handle = Handle
  { jwtSettings :: JWTSettings
  , cookieSettings :: CookieSettings
  , nextId :: IORef GenericId
  , cache :: IORef (Map GenericId Text)
  }

newHandle :: IO Handle
newHandle = Handle
  -- TODO(joachifm) persist key (readKey/writeKey)
  <$> (defaultJWTSettings <$> generateKey)
  <*> pure defaultCookieSettings
  <*> newIORef 1
  <*> newIORef mempty

getNextId :: Handle -> IO GenericId
getNextId = postIncIORef . nextId

------------------------------------------------------------------------

noContent
  :: IO a
  -> IO NoContent
noContent
  = (*> pure NoContent)

redirect
  :: Text
  -> orig
  -> IO (Headers '[Header "Location" Text] orig)
redirect loc orig
  = pure $ addHeader loc orig

------------------------------------------------------------------------

text_ :: Text -> Html ()
text_ = toHtml

simplePage
  :: Text
  -> Html ()
  -> Html ()
simplePage pageTitle pageBody = doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "UTF-8" ]
    title_ pageTitle_
  body_ $ do
    renderNav
    h1_ pageTitle_
    div_ [ class_ "main" ] $ do
      pageBody
  where
    pageTitle_ = text_ pageTitle

renderNav :: Html ()
renderNav = nav_ $ ul_ $ do
  li_ $ a_ [ href_ "/" ]      (text_ "Home")
  li_ $ a_ [ href_ "/login" ] (text_ "Login")

------------------------------------------------------------------------

renderIndexPage :: Html ()
renderIndexPage = simplePage "Liquidator" $ do
  p_ $ text_ "Hello, there"

renderLoginPage :: Html ()
renderLoginPage = simplePage "Login" $ do
  form_ [ name_ "login"
        , method_ "post"
        , action_ "/login" -- TODO(joachifm) use API link
        ] $ do
    section_ $ do
      input_ [ name_ "username"
             , type_ "text"
             , placeholder_ "Username"
             , required_ "required"
             , autofocus_
             , tabindex_ "1"
             ]
      input_ [ name_ "password"
             , type_ "password"
             , placeholder_ "Passphrase"
             , required_ "required"
             , tabindex_ "2"
             ]
    input_ [ type_ "submit"
           , value_ "Login"
             , tabindex_ "3"
           ]

------------------------------------------------------------------------

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

formOptions :: FormOptions
formOptions = defaultFormOptions
  { fieldLabelModifier = Aeson.fieldLabelModifier aesonOptions
  }

------------------------------------------------------------------------

data UserSession = UserSession { sessionId :: GenericId }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

------------------------------------------------------------------------

data LoginFormData = LoginFormData
  { username :: Text
  , password :: Text
  }
  deriving (Show, Generic, FromForm, ToForm)

------------------------------------------------------------------------

type WebApi
  =    Get '[HTML] (Html ())
  :<|> "login" :>
       Get '[HTML] (Html ())
  :<|> "login" :>
       ReqBody '[FormUrlEncoded] LoginFormData :>
       Verb 'POST 301 '[PlainText] (Headers '[ Header "Location" Text
                                               -- XSRF-token & JWT-cookie
                                             , Header "Set-Cookie" SetCookie
                                             , Header "Set-Cookie" SetCookie
                                             ] NoContent)

------------------------------------------------------------------------

getIndexPageHandler
  :: Handle
  -> IO (Html ())
getIndexPageHandler _ = pure $ renderIndexPage

getLoginPageHandler
  :: Handle
  -> IO (Html ())
getLoginPageHandler _ = pure $ renderLoginPage

postLoginHandler
  :: Handle
  -> LoginFormData
  -> IO (Headers '[ Header "Location"  Text
                  , Header "Set-Cookie" SetCookie
                  , Header "Set-Cookie" SetCookie
                  ] NoContent)
postLoginHandler h rq = do
  if (username rq == "admin" && password rq == "admin")
    then do
      let sess = UserSession 42
      mApplyCookies <- acceptLogin (cookieSettings h) (jwtSettings h) sess
      case mApplyCookies of
        Nothing           -> E.throwIO err401
        Just applyCookies -> return $ addHeader "/" (applyCookies NoContent)
    else
      E.throwIO err401

webHandler
  :: Handle
  -> ServerT WebApi IO
webHandler h
  =    getIndexPageHandler h
  :<|> getLoginPageHandler h
  :<|> postLoginHandler h

----------------------------------------------------------------------------

type Api
  =    WebApi
  :<|> "api" :> "v1.0" :> Raw

handler :: Handle -> ServerT Api IO
handler h
  =    webHandler h
  :<|> serveDirectoryFileServer "static"

----------------------------------------------------------------------------

-- | A natural transformation from our preferred handler context to servant's
-- 'Servant.API.Handler'.
convert
  :: IO a
  -> Handler a
convert = Handler . ExceptT . E.try

----------------------------------------------------------------------------

api :: Proxy Api
api = Proxy

server :: Handle -> Server Api
server = hoistServer api convert . handler

app :: IO Application
app = serve api . server <$> newHandle

----------------------------------------------------------------------------

run :: IO ()
run
  = Warp.runTLS tlsSettings warpSettings
  . Gzip.gzip Gzip.def
  . ForceSSL.forceSSL
  . hsts
  =<< app

tlsSettings :: Warp.TLSSettings
tlsSettings = (Warp.tlsSettings "site.crt" "site.key")
  { Warp.onInsecure = Warp.AllowInsecure -- Upgraded by forceSSL
  }

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

prodSettings :: (Warp.Settings -> Warp.Settings)
prodSettings
  = Warp.setPort 80
  . Warp.setHost "*"

------------------------------------------------------------------------
-- Middleware: HTTP Strict Transport Security
------------------------------------------------------------------------

hsts :: Middleware
hsts = AddHeaders.addHeaders [
  ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
  ]
