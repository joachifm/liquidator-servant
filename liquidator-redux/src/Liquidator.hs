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

import Control.Monad (join, forM_, when)
import Control.Monad.Except (ExceptT(ExceptT))
import qualified Control.Exception as E

import qualified Control.Lens as Lens
import Control.Lens.Operators
import Control.Lens.TH

import Data.Maybe (fromMaybe)

import Data.Int (Int64)

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Text.Encoding as Text
import qualified Data.ByteString.Lazy as LB

import Data.Map (Map)
import qualified Data.Map.Lazy as Map

import Data.IORef
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  , writeIORef
  )

import Data.Time.Calendar (Day)
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

import Web.FormUrlEncoded (FromForm, ToForm)
import qualified Web.FormUrlEncoded as Form
import Web.Cookie

import Network.HTTP.Types.Method

import Servant

import Data.Swagger (Swagger)
import qualified Data.Swagger as Swagger
import Servant.Swagger

import Lucid (Html, toHtml)
import Lucid.Html5
import Servant.HTML.Lucid

import Servant.Auth.Server

import Network.Wai (Middleware)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import qualified Network.Wai.Middleware.ForceSSL as ForceSSL
import qualified Network.Wai.Middleware.Gzip as Gzip

import System.Log.FastLogger
import System.Directory (renamePath)

import IORef
import Util
import qualified Wai.Middleware.Hsts as Hsts
import qualified Wai.Middleware.VerifyCsrToken as VerifyCsrToken

------------------------------------------------------------------------------

type GenericId = Int64

------------------------------------------------------------------------------

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

formOptions :: Form.FormOptions
formOptions = Form.defaultFormOptions
  { Form.fieldLabelModifier = Aeson.fieldLabelModifier aesonOptions
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
  <*> pure (defaultCookieSettings { cookieXsrfSetting = Just xsrfCookieSettings })
  <*> newStdoutLoggerSet defaultBufSize
  <*> newIORef 1
  <*> newIORef mempty
  <*> newIORef mempty
  where
    xsrfCookieSettings = defaultXsrfCookieSettings { xsrfExcludeGet = True }

    getJwk path = readKey path `E.catch` \(_::E.IOException) -> writeKey path >> readKey path

getNextId :: Handle -> IO GenericId
getNextId = postIncIORef . _nextId

data SavedState = SavedState
  { savedstateNextId :: GenericId
  , savedstateTransactions :: Map GenericId Text
  }
  deriving (Generic)

instance FromJSON SavedState where
  parseJSON = Aeson.genericParseJSON aesonOptions

instance ToJSON SavedState where
  toJSON = Aeson.genericToJSON aesonOptions
  toEncoding = Aeson.genericToEncoding aesonOptions

saveHandleState
  :: Handle
  -> IO SavedState
saveHandleState h = SavedState
  <$> readIORef (h^.nextId)
  <*> readIORef (h^.transactions)

restoreHandleState
  :: Handle
  -> SavedState
  -> IO ()
restoreHandleState h s = do
  writeIORef (h^.nextId) (savedstateNextId s)
  writeIORef (h^.transactions) (savedstateTransactions s)

dumpState
  :: Handle
  -> FilePath
  -> IO ()
dumpState h outFile = do
  let
    tmpFile = outFile <> ".tmp"
  Aeson.encodeFile tmpFile =<< saveHandleState h
  renamePath tmpFile outFile

loadState
  :: Handle
  -> FilePath
  -> IO ()
loadState h inFile = do
  s <- Aeson.decodeFileStrict' inFile `E.catch` \(_::E.IOException) -> return Nothing
  case s of
    Just v  -> restoreHandleState h v
    Nothing -> return ()

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

-- https://stackoverflow.com/a/54890919
newtype Cookies' = Cookies' { unCookies :: Cookies }

instance FromHttpApiData Cookies' where
  parseHeader = return . Cookies' . parseCookies
  parseQueryParam = return . Cookies' . parseCookies . Text.encodeUtf8

------------------------------------------------------------------------------

text_ :: Text -> Html ()
text_ = toHtml

simplePage'
  :: Text
  -> Maybe Text
  -> Html ()
  -> Html ()
simplePage' pageTitle mbPageXsrfToken pageBody = doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "UTF-8" ]
    whenJust mbPageXsrfToken $ \pageXsrfToken ->
      meta_ [ name_ "xsrf-token", value_ pageXsrfToken ]
    title_ pageTitle_
  body_ $ do
    renderNav
    h1_ pageTitle_
    div_ [ class_ "main" ] $ do
      pageBody
  where
    pageTitle_ = text_ pageTitle

simplePage
  :: Text
  -> Html ()
  -> Html ()
simplePage pageTitle pageBody = simplePage' pageTitle Nothing pageBody

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
  -> Maybe Text
  -> Html ()
renderNewTransactionPage _ mbXsrfToken = simplePage' "New" mbXsrfToken $ do
  form_ [ name_ "new"
        , method_ "post"
        , action_ "/new" -- TODO(joachifm) use API link
        ] $ do
    section_ $ do
      input_ [ name_ "subject"
             , type_ "text"
             , placeholder_ "Subject"
             , required_ "required"
             , spellcheck_ "true"
             , autofocus_
             , tabindex_ "1"
             ]
      input_ [ name_ "amount_pri"
             , type_ "number"
             , placeholder_ "0"
             , min_ "0"
             , max_ "9999"
             , required_ "required"
             , tabindex_ "2"
             ]
      input_ [ name_ "amount_sub"
             , type_ "number"
             , placeholder_ "0"
             , min_ "0"
             , max_ "99"
             , value_ "0"
             , tabindex_ "3"
             ]
      input_ [ name_ "day"
             , type_ "date"
             , required_ "required"
             , value_ "2019-01-01"
             , tabindex_ "4"
             ]

    -- Pass along value for X-XSRF-TOKEN header
    whenJust mbXsrfToken $ \v -> do
      input_ [ name_ "xsrf_token"
             , type_ "text"
             , hidden_ "hidden"
             , value_ v
             ]

    input_ [ type_ "submit"
           , value_ "Create"
           , tabindex_ "5"
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
  , createtransactionformAmountSub :: Maybe Int
  , createtransactionformDay :: Day
  , createtransactionformXsrfToken :: Text -- X-XSRF-TOKEN header value
  }
  deriving (Generic, Show)

instance FromForm CreateTransactionFormData where
  fromForm = Form.genericFromForm formOptions

data LoginFormData = LoginFormData
  { loginformUsername :: Text
  , loginformPassword :: Text
  }
  deriving (Generic)

instance Show LoginFormData where
  show _ = "LoginFormData { loginformUsername = \"<hidden>\", loginformPassword = \"<hidden>\" }"

instance FromForm LoginFormData where
  fromForm = Form.genericFromForm formOptions

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
  , _transactionDay :: Day
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
       Header "Cookie" Cookies' :>
       "new" :>
       Get '[HTML]
           (Html ())

  :<|> Auth '[Cookie] UserSession :>
       "new" :>
       ReqBody '[FormUrlEncoded] CreateTransactionFormData :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text ]
                     NoContent)

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

lookupXsrfTokenText
  :: Maybe XsrfCookieSettings
  -> Cookies'
  -> Maybe Text
lookupXsrfTokenText Nothing _
  = Nothing
lookupXsrfTokenText (Just settings) cookies
  = Text.decodeUtf8 <$> lookup (xsrfCookieName settings)
                               (unCookies cookies)

getNewTransactionPageHandler
  :: Handle
  -> AuthResult UserSession
  -> Maybe Cookies'
  -> IO (Html ())
getNewTransactionPageHandler h (Authenticated sess) mbCookies = do
  print (unCookies <$> mbCookies)
  pure . renderNewTransactionPage sess
       $ join (lookupXsrfTokenText (cookieXsrfSetting (h^.cookieSettings)) <$>
                                   mbCookies)
getNewTransactionPageHandler _ _ _
  = E.throwIO err401

postNewTransactionHandler
  :: Handle
  -> AuthResult UserSession
  -> CreateTransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postNewTransactionHandler h (Authenticated sess) formData = do
  _ <- addTransaction h $
    AddTransactionParam
      (createtransactionformSubject formData)
      ((createtransactionformAmountPri formData
       ,fromMaybe (0::Int) (createtransactionformAmountSub formData)))
      (createtransactionformDay formData)
  pure . addHeader "/list"
       $ NoContent
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

appWith :: Handle -> Application
appWith h = VerifyCsrToken.verifyCsrToken $
  serveWithContext api (authContextVal h) (server h)

app :: IO Application
app = appWith <$> newHandle

withApp
  :: (Application -> IO a)
  -> IO a
withApp act = E.bracket
  (newHandle >>= \h -> loadState h "_liquidator.state" >> return h)
  (\h -> dumpState h "_liquidator.state")
  (\h -> act $ appWith h)

----------------------------------------------------------------------------

run :: IO ()
run = withApp runWith

runWith :: Application -> IO ()
runWith
  = Warp.runTLS tlsSettings warpSettings
  . Gzip.gzip Gzip.def
  . ForceSSL.forceSSL
  . Hsts.hsts

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
