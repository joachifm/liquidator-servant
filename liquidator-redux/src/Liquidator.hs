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
import qualified Data.Text.Encoding as Text
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

import Data.Time.Clock
  ( UTCTime
  , getCurrentTime
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

import qualified Network.Wai.Middleware.ForceSSL as ForceSSL
import qualified Network.Wai.Middleware.Gzip as Gzip

import Options.Applicative

import IORef
--import Money
import qualified Wai.Middleware.Hsts as Hsts

------------------------------------------------------------------------------

type GenericId = Int64

------------------------------------------------------------------------------

data Config = Config
  { jwtKeyFile :: FilePath
  , tlsCrtFile :: FilePath
  , tlsKeyFile :: FilePath
  }
  deriving (Show, Generic, FromJSON, ToJSON)

defaultConfig
  :: Config
defaultConfig = Config
  { jwtKeyFile = "server.jwk"
  , tlsCrtFile = "server.crt"
  , tlsKeyFile = "server.key"
  }

data Handle = Handle
  { jwtSettings :: JWTSettings
  , cookieSettings :: CookieSettings
  , nextId :: IORef GenericId
  , cache :: IORef (Map GenericId Text)
  , transactions :: IORef (Map GenericId Text)
  }

newHandle :: IO Handle
newHandle = Handle
  -- TODO(joachifm) persist JWT key (readKey/writeKey)
  <$> (defaultJWTSettings <$> generateKey)
  <*> pure (defaultCookieSettings { cookieXsrfSetting = Just xsrfCookieSettings })
  <*> newIORef 1
  <*> newIORef mempty
  <*> newIORef mempty
  where
    xsrfCookieSettings = defaultXsrfCookieSettings { xsrfExcludeGet = True }

getNextId :: Handle -> IO GenericId
getNextId = postIncIORef . nextId

------------------------------------------------------------------------------

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

------------------------------------------------------------------------------

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
  li_ $ a_ [ href_ "/list" ]  (text_ "List")

------------------------------------------------------------------------------

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

renderTransactionsListPage
  :: Html ()
renderTransactionsListPage = simplePage "Transactions" $ do
  p_ "Nil"

------------------------------------------------------------------------------

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

formOptions :: FormOptions
formOptions = defaultFormOptions
  { fieldLabelModifier = Aeson.fieldLabelModifier aesonOptions
  }

------------------------------------------------------------------------------

data UserSession = UserSession { sessionId :: GenericId }
  deriving (Show, Eq, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

------------------------------------------------------------------------------

data LoginFormData = LoginFormData
  { loginformUsername :: Text
  , loginformPassword :: Text
  }
  deriving (Show, Generic)

instance FromForm LoginFormData where
  fromForm = genericFromForm formOptions

loginFormDataToBasicAuthData
  :: LoginFormData
  -> BasicAuthData
loginFormDataToBasicAuthData formData
  = BasicAuthData
    { basicAuthUsername = Text.encodeUtf8 (loginformUsername formData)
    , basicAuthPassword = Text.encodeUtf8 (loginformPassword formData)
    }

------------------------------------------------------------------------------

type WebApi
  =    Get '[HTML] (Html ())
  :<|> "login" :>
       Get '[HTML] (Html ())
  :<|> "login" :>
       ReqBody '[FormUrlEncoded] LoginFormData :>
       Verb 'POST 301 '[PlainText] (Headers '[ Header "Location" Text
                                               -- XSRF-TOKEN & JWT-COOKIE
                                             , Header "Set-Cookie" SetCookie
                                             , Header "Set-Cookie" SetCookie
                                             ] NoContent)
  :<|> Auth '[Cookie] UserSession :>
       "list" :>
       Get '[HTML] (Html ())

------------------------------------------------------------------------------

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
  -> IO (Headers '[ Header "Location" Text
                  , Header "Set-Cookie" SetCookie
                  , Header "Set-Cookie" SetCookie
                  ] NoContent)
postLoginHandler h rq = do
  if loginformUsername rq == "admin" && loginformPassword rq == "admin"
    then do
      let sess = UserSession 42
      applyCookies <- acceptLogin (cookieSettings h) (jwtSettings h) sess
      case applyCookies of
        Just fn ->
          pure . addHeader "/"
               . fn
               $ NoContent
        Nothing ->
          E.throwIO err401
    else
      E.throwIO err401

getTransactionsListPageHandler
  :: Handle
  -> AuthResult UserSession
  -> IO (Html ())
getTransactionsListPageHandler _ (Authenticated _)
  = pure $ renderTransactionsListPage
getTransactionsListPageHandler _ _
  = E.throwIO err401

webHandler
  :: Handle
  -> ServerT WebApi IO
webHandler h
  =    getIndexPageHandler h
  :<|> getLoginPageHandler h
  :<|> postLoginHandler h
  :<|> getTransactionsListPageHandler h

------------------------------------------------------------------------------

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

authContextProxy
  :: Proxy '[CookieSettings, JWTSettings]
authContextProxy = Proxy

authContextVal
  :: Handle
  -> Context '[CookieSettings, JWTSettings]
authContextVal h
  =  cookieSettings h
  :. jwtSettings h
  :. EmptyContext

api :: Proxy Api
api = Proxy

server :: Handle -> Server Api
server = hoistServerWithContext api authContextProxy convert . handler

app :: IO Application
app = do
  h <- newHandle
  return $ serveWithContext api (authContextVal h) (server h)

----------------------------------------------------------------------------

run :: IO ()
run
  = Warp.runTLS tlsSettings warpSettings
  . Gzip.gzip Gzip.def
  . ForceSSL.forceSSL
  . Hsts.hsts
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
