{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator where

import GHC.Generics (Generic)

import Control.Monad (forM_, when)
import Control.Monad.Except (ExceptT(ExceptT))
import qualified Control.Exception as E

import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Lens.TH

import Data.Int (Int64)

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import Data.Map (Map)
import qualified Data.Map.Lazy as Map

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

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import qualified Data.Aeson as Aeson

import Web.FormUrlEncoded

import Servant

import Data.Swagger (Swagger)
import qualified Data.Swagger as Swagger
import Servant.Swagger

import Lucid (Html, toHtml)
import Lucid.Html5
import Servant.HTML.Lucid

import Servant.Auth.Server

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import qualified Network.Wai.Middleware.ForceSSL as ForceSSL
import qualified Network.Wai.Middleware.Gzip as Gzip

import System.Log.FastLogger

import IORef
import qualified Wai.Middleware.Hsts as Hsts

------------------------------------------------------------------------------

showText :: (Show a) => a -> Text
showText = Text.pack . show

------------------------------------------------------------------------------

type GenericId = Int64

------------------------------------------------------------------------------

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

formOptions :: FormOptions
formOptions = defaultFormOptions
  { fieldLabelModifier = Aeson.fieldLabelModifier aesonOptions
  }

------------------------------------------------------------------------------

data Role
  = RoleAdmin
  | RoleOwner
  | RoleUser
  deriving (Eq, Enum, Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

makePrisms ''Role

data UserSessionData = UserSessionData
  { usersessionUsername :: Text
  }
  deriving (Eq, Generic)

instance FromJSON UserSessionData where
  parseJSON = Aeson.genericParseJSON aesonOptions

instance ToJSON UserSessionData where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

data UserSession = UserSession { sessionId :: GenericId }
  deriving (Eq, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

------------------------------------------------------------------------------

data Config = Config
  { _jwtKeyFile :: FilePath
  , _tlsCrtFile :: FilePath
  , _tlsKeyFile :: FilePath
  }
  deriving (Show, Generic, FromJSON, ToJSON)

makeLenses ''Config

defaultConfig
  :: Config
defaultConfig = Config
  { _jwtKeyFile = "server.jwk"
  , _tlsCrtFile = "server.crt"
  , _tlsKeyFile = "server.key"
  }

data Handle = Handle
  { _config :: Config
  , _jwtSettings :: JWTSettings
  , _cookieSettings :: CookieSettings
  , _logger :: LoggerSet

  , _nextId :: IORef GenericId
  , _sessions :: IORef (Map GenericId UserSessionData)
  , _transactions :: IORef (Map GenericId Text)
  }

makeLenses ''Handle

newHandle :: IO Handle
newHandle = newHandleWith defaultConfig

newHandleWith :: Config -> IO Handle
newHandleWith cfg = Handle
  <$> pure cfg
  <*> (defaultJWTSettings <$> getJwk (cfg ^. jwtKeyFile))
  -- TODO(joachifm) use js to inject xsrf setting on the client side?
  <*> pure (defaultCookieSettings { cookieXsrfSetting = Nothing })
  <*> newStdoutLoggerSet defaultBufSize
  <*> newIORef 1
  <*> newIORef mempty
  <*> newIORef mempty
  where
    getJwk path = readKey path `E.catch` \(_::E.IOException) -> writeKey path >> readKey path

getNextId :: Handle -> IO GenericId
getNextId = postIncIORef . _nextId

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
  li_ $ a_ [ href_ "/" ] (text_ "Home")
  li_ $ a_ [ href_ "/login" ] (text_ "Login")
  li_ $ a_ [ href_ "/logout" ] (text_ "Logout")
  li_ $ a_ [ href_ "/list" ] (text_ "List")
  li_ $ a_ [ href_ "/new" ] (text_ "New")

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

renderNewTransactionPage
  :: UserSession
  -> Html ()
renderNewTransactionPage _ = simplePage "New" $ do
  form_ [ name_ "new"
        , method_ "post"
        , action_ "/new" -- TODO(joachifm) use API link
        ] $ do
    section_ $ do
      input_ [ name_ "subject"
             , type_ "text"
             , placeholder_ "Subject"
             , required_ "required"
             , autofocus_
             , tabindex_ "1"
             ]
      input_ [ name_ "amount_pri"
             , type_ "number"
             , placeholder_ "Amount"
             , min_ "0"
             , max_ "9999"
             , required_ "required"
             , tabindex_ "2"
             ]
      input_ [ name_ "amount_sub"
             , type_ "number"
             , placeholder_ "Amount"
             , min_ "0"
             , max_ "9999"
             , value_ "0"
             , tabindex_ "3"
             ]
    input_ [ type_ "submit"
           , value_ "Create"
           , tabindex_ "4"
           ]

renderTransactionsListPage
  :: UserSession
  -> [(GenericId, Text)]
  -> Html ()
renderTransactionsListPage sess txlist = simplePage "Transactions" $ do
  p_ $ do
    text_ ("The session id is : " <> showText (sessionId sess))
  ul_ $ do
    forM_ txlist $ \(i, tx) -> do
      li_ (text_ (showText i <> ":" <> tx))

------------------------------------------------------------------------------

data CreateTransactionFormData = CreateTransactionFormData
  { createtransactionformSubject :: Text
  , createtransactionformAmountPri :: Int
  , createtransactionformAmountSub :: Int
  }
  deriving (Generic, Show)

instance FromForm CreateTransactionFormData where
  fromForm = genericFromForm formOptions

data LoginFormData = LoginFormData
  { loginformUsername :: Text
  , loginformPassword :: Text
  }
  deriving (Generic)

instance Show LoginFormData where
  show _ = "LoginFormData { loginformUsername = \"<hidden>\", loginformPassword = \"<hidden>\" }"

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

data AddTransactionParam = AddTransactionParam
  { _transactionSubject :: Text
  , _transactionAmount :: (Int, Int)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

makeLenses ''AddTransactionParam

getAllTransactions
  :: Handle
  -> IO [(GenericId, Text)]
getAllTransactions h = Map.toList <$> readIORef (h^.transactions)

addTransaction
  :: Handle
  -> AddTransactionParam
  -> IO GenericId
addTransaction h param = do
  theid <- getNextId h
  atomicModifyIORef' (h^.transactions) $ \m ->
    (Map.insert theid (param^.transactionSubject) m, theid)

------------------------------------------------------------------------------

type WebApi
  =    Get '[HTML] (Html ())

  -- /login
  :<|> "login" :>
       Get '[HTML] (Html ())

  :<|> "login" :>
       ReqBody '[FormUrlEncoded] LoginFormData :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text
                        -- XSRF-TOKEN & JWT-COOKIE
                      , Header "Set-Cookie" SetCookie
                      , Header "Set-Cookie" SetCookie
                      ] NoContent)

  -- /logout
  :<|> "logout" :>
       Get '[HTML] (Html ())

  :<|> "logout" :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text
                      , Header "Set-Cookie" SetCookie
                      , Header "Set-Cookie" SetCookie
                      ] NoContent)

  -- /list
  :<|> Auth '[Cookie] UserSession :>
       "list" :>
       Get '[HTML] (Html ())

  -- /new
  :<|> Auth '[Cookie] UserSession :>
       "new" :>
       Get '[HTML] (Html ())

  :<|> Auth '[Cookie] UserSession :>
       "new" :>
       ReqBody '[FormUrlEncoded] CreateTransactionFormData :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text ] NoContent)

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
      applyCookies <- acceptLogin (h^.cookieSettings) (h^.jwtSettings) sess
      case applyCookies of
        Just fn ->
          pure . addHeader "/"
               . fn
               $ NoContent
        Nothing ->
          E.throwIO err401
    else
      E.throwIO err401

getLogoutPageHandler
  :: Handle
  -> IO (Html ())
getLogoutPageHandler _ = pure . simplePage "Log out" $ do
  form_ [ name_ "logout"
        , action_ "/logout"
        , method_ "post"
        ] $ do
    input_ [ type_ "submit", value_ "Log out" ]

postLogoutHandler
  :: Handle
  -> IO (Headers '[ Header "Location" Text
                  , Header "Set-Cookie" SetCookie
                  , Header "Set-Cookie" SetCookie
                  ] NoContent)
postLogoutHandler h
  = pure . addHeader "/"
  . clearSession (h^.cookieSettings)
  $ NoContent

getNewTransactionPageHandler
  :: Handle
  -> AuthResult UserSession
  -> IO (Html ())
getNewTransactionPageHandler _ (Authenticated sess)
  = pure $ renderNewTransactionPage sess
getNewTransactionPageHandler _ _
  = E.throwIO err401

postNewTransactionHandler
  :: Handle
  -> AuthResult UserSession
  -> CreateTransactionFormData
  -> IO (Headers '[ Header "Location" Text ] NoContent)
postNewTransactionHandler h (Authenticated sess) formData = do
  theid <- addTransaction h $
    AddTransactionParam
      (createtransactionformSubject formData)
      ((createtransactionformAmountPri formData
       ,createtransactionformAmountSub formData))
  putStrLn ("Created txid: " <> show theid)
  pure . addHeader "/" $ NoContent
postNewTransactionHandler _ _ _ = E.throwIO err401

getTransactionsListPageHandler
  :: Handle
  -> AuthResult UserSession
  -> IO (Html ())
getTransactionsListPageHandler h (Authenticated sess)
  = renderTransactionsListPage sess <$> getAllTransactions h
getTransactionsListPageHandler _ _
  = E.throwIO err401

webHandler
  :: Handle
  -> ServerT WebApi IO
webHandler h
  =    getIndexPageHandler h
  :<|> getLoginPageHandler h
  :<|> postLoginHandler h
  :<|> getLogoutPageHandler h
  :<|> postLogoutHandler h
  :<|> getTransactionsListPageHandler h
  :<|> getNewTransactionPageHandler h
  :<|> postNewTransactionHandler h

------------------------------------------------------------------------------

type Api
  =    WebApi
  :<|> "api" :> "v1.0" :> EmptyAPI

handler :: Handle -> ServerT Api IO
handler h
  =    webHandler h
  :<|> emptyServer

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
  =  h^.cookieSettings
  :. h^.jwtSettings
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
