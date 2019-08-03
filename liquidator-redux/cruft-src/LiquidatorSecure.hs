{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module LiquidatorSecure where

import Imports

import Control.Lens.Operators
import Control.Lens.TH
import Control.Monad.Except (ExceptT(ExceptT))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day)
import Lucid (Html, toHtml)
import Lucid.Html5
import Servant
import Servant.Auth.Server
import Servant.HTML.Lucid
import System.Directory (renamePath)
import System.Log.FastLogger
import Web.Cookie
import Web.FormUrlEncoded (FromForm)
import qualified Control.Exception as E
import qualified Data.Aeson as Aeson
import qualified Data.Map.Lazy as Map
import qualified Data.Text.Encoding as Text
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import qualified Network.Wai.Middleware.ForceSSL as ForceSSL
import qualified Network.Wai.Middleware.Gzip as Gzip
import qualified Web.FormUrlEncoded as Form

import IORef
import Money
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

data Transaction = Transaction
  { transactionSubject :: Text
  , transactionAmount :: Money
  , transactionDay :: Day
  }
  deriving
    ( Generic
    , FromJSON
    , ToJSON
#ifdef TEST
    , Show
#endif
    )

data Role
  = RoleAdmin
  | RoleOwner
  | RoleUser
  deriving
    ( Eq
    , Enum
    , Generic
    , FromJSON
    , ToJSON
    , FromJWT
    , ToJWT
#ifdef TEST
    , Show
#endif
    )

makePrisms ''Role

data UserSession = UserSession { sessionId :: GenericId }
  deriving (Eq, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

------------------------------------------------------------------------------

data Config = Config
  { _jwtKeyFile :: FilePath
  , _tlsCrtFile :: FilePath
  , _tlsKeyFile :: FilePath
  }
  deriving
    ( Generic
    , FromJSON
    , ToJSON
#ifdef TEST
    , Show
#endif
    )

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
  , _transactions :: IORef (Map GenericId Transaction)
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
  where
    xsrfCookieSettings = defaultXsrfCookieSettings { xsrfExcludeGet = True }

    getJwk path = readKey path `E.catch` \(_::E.IOException) -> writeKey path >> readKey path

getNextId :: Handle -> IO GenericId
getNextId = postIncIORef . _nextId

data SavedState = SavedState
  { savedstateNextId :: GenericId
  , savedstateTransactions :: Map GenericId Transaction
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
  writeIORef (h^.nextId)       (savedstateNextId s)
  writeIORef (h^.transactions) (savedstateTransactions s)

dumpState
  :: Handle
  -> FilePath
  -> IO ()
dumpState h outFile = do
  let tmpFile = outFile <> ".tmp"
  Aeson.encodeFile tmpFile =<< saveHandleState h
  renamePath tmpFile outFile

loadState
  :: Handle
  -> FilePath
  -> IO ()
loadState h inFile = do
  s <- Aeson.decodeFileStrict' inFile
       `E.catch` \(_::E.IOException) -> return Nothing
  case s of
    Just v  -> restoreHandleState h v
    Nothing -> return ()

withHandle
  :: (Handle -> IO a)
  -> IO a
withHandle act = E.bracket
  (newHandle >>= \h -> loadState h stateFile >> return h)
  (flip dumpState stateFile)
  act
  where
    stateFile = "_liquidator.state"

------------------------------------------------------------------------------

getAllTransactions
  :: Handle
  -> IO [(GenericId, Transaction)]
getAllTransactions h
  = Map.toList <$> readIORef (h^.transactions)

getTransactionById
  :: Handle
  -> GenericId
  -> IO (Maybe Transaction)
getTransactionById h txid
  = Map.lookup txid <$> readIORef (h^.transactions)

data AddTransactionParam = AddTransactionParam
  { _addTransactionParamSubject :: Text
  , _addTransactionParamAmount :: (MoneyAmount, MoneyAmount)
  , _addTransactionParamDay :: Day
  }
#ifdef TEST
  deriving (Show)
#endif

makeLenses ''AddTransactionParam

addTransaction
  :: Handle
  -> AddTransactionParam
  -> IO GenericId
addTransaction h param = do
  theid <- getNextId h
  atomicModifyIORef' (h^.transactions) $ \m ->
    let
      tx = Transaction
        { transactionSubject = param^.addTransactionParamSubject
        , transactionAmount = uncurry moneyFromAmount (param^.addTransactionParamAmount)
        , transactionDay = param^.addTransactionParamDay
        }
    in
      (Map.insert theid tx m, theid)

------------------------------------------------------------------------------

noContent
  :: IO a
  -> IO NoContent
noContent
  = (*> pure NoContent)

-- https://stackoverflow.com/a/54890919
newtype Cookies' = Cookies' { unCookies :: Cookies }

instance FromHttpApiData Cookies' where
  parseHeader     = return . Cookies' . parseCookies
  parseQueryParam = return . Cookies' . parseCookies . Text.encodeUtf8

------------------------------------------------------------------------------

text_ :: Text -> Html ()
text_ = toHtml

------------------------------------------------------------------------------

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
    footer_ $ do
      text_ ("XSRF-TOKEN: " <> showText mbPageXsrfToken)
  where
    pageTitle_ = text_ pageTitle

simplePage
  :: Text
  -> Html ()
  -> Html ()
simplePage pageTitle pageBody = simplePage' pageTitle Nothing pageBody

renderNav :: Html ()
renderNav = nav_ $ do
  span_ $ a_ [ href_ "/" ]       (text_ "Home")   >> text_ " | "
  span_ $ a_ [ href_ "/login" ]  (text_ "Login")  >> text_ " | "
  span_ $ a_ [ href_ "/logout" ] (text_ "Logout") >> text_ " | "
  span_ $ a_ [ href_ "/list" ]   (text_ "List")   >> text_ " | "
  span_ $ a_ [ href_ "/new" ]    (text_ "New")

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
             , max_ "999999"
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
  -> [(GenericId, Transaction)]
  -> Html ()
renderTransactionsListPage sess txlist = simplePage "Transactions" $ do
  p_ $ do
    text_ ("The session id is : " <> showText (sessionId sess))
  ul_ $ do
    forM_ txlist $ \(i, tx) -> do
      li_ $ do
        a_ [ href_ ("/view/" <> showText i) ] $
          text_ (showText i <> ":" <> transactionSubject tx)

renderViewTransactionByIdPage
  :: GenericId
  -> Html ()
renderViewTransactionByIdPage txid = simplePage "View" $ do
  p_ $ do
    text_ ("Viewing txid " <> showText txid)
  p_ $ do
    a_ [ href_ ("/edit/" <> showText txid) ] $
      text_ "Edit"

renderEditTransactionByIdPage
  :: Maybe Text
  -> GenericId
  -> Html ()
renderEditTransactionByIdPage mbXsrfToken txid = simplePage' "Edit" mbXsrfToken $ do
  p_ $ do
    text_ ("Editing txid " <> showText txid)
  div_ $ do
    form_ [ name_ "edit"
          , action_ "/edit"
          , method_ "post"
          ] $ do
      -- TODO(joachifm) xsrf token hidden field
      section_ $ do
        input_ [ name_ "subject"
               , type_ "text"
               , value_ "Subject"
               , required_ "required"
               , autofocus_
               , tabindex_ "1"
               ]

        input_ [ name_ "txid"
               , type_ "number"
               , value_ (showText txid)
               , hidden_ "hidden"
               ]

      whenJust mbXsrfToken $ \xsrfToken ->
        input_ [ name_ "xsrf_token"
               , type_ "text"
               , value_ xsrfToken
               , hidden_ "hidden"
               ]

      input_ [ type_ "submit"
             , value_ "Apply changes"
             , tabindex_ "2"
             ]

------------------------------------------------------------------------------

data CreateTransactionFormData = CreateTransactionFormData
  { createtransactionformSubject :: Text
  , createtransactionformAmountPri :: Int
  , createtransactionformAmountSub :: Maybe Int
  , createtransactionformDay :: Day
  , createtransactionformXsrfToken :: Text -- X-XSRF-TOKEN header value
  }
  deriving (Generic)

instance FromForm CreateTransactionFormData where
  fromForm = Form.genericFromForm formOptions

data EditTransactionFormData = EditTransactionFormData
  { edittransactionformSubject :: Maybe Text
  , edittransactionformAmountPri :: Maybe Int
  , edittransactionformAmountSub :: Maybe Int
  , edittransactionformDay :: Maybe Day
  , edittransactionformXsrfToken :: Text -- X-XSRF-TOKEN header value
  }
  deriving (Generic)

instance FromForm EditTransactionFormData where
  fromForm = Form.genericFromForm formOptions

data LoginFormData = LoginFormData
  { loginformUsername :: Text
  , loginformPassword :: Text
  }
  deriving (Generic)

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

  -- /view
  :<|> Auth '[Cookie] UserSession :>
       "view" :>
       Capture "id" GenericId :>
       Get '[HTML]
           (Html ())

  -- /edit
  :<|> Auth '[Cookie] UserSession :>
       Header "Cookie" Cookies' :>
       "edit" :>
       Capture "id" GenericId :>
       Get '[HTML]
           (Html ())

  :<|> Auth '[Cookie] UserSession :>
       "edit" :>
       ReqBody '[FormUrlEncoded] EditTransactionFormData :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text ]
                     NoContent)

------------------------------------------------------------------------------

lookupXsrfTokenText
  :: Maybe XsrfCookieSettings
  -> Cookies'
  -> Maybe Text
lookupXsrfTokenText Nothing _
  = Nothing
lookupXsrfTokenText (Just settings) cookies
  = Text.decodeUtf8 <$> lookup (xsrfCookieName settings)
                               (unCookies cookies)

------------------------------------------------------------------------------

-- /

getIndexPageHandler
  :: Handle
  -> IO (Html ())
getIndexPageHandler _ = pure $ renderIndexPage

-- /login

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

-- /logout

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

-- /new

getNewTransactionPageHandler
  :: Handle
  -> AuthResult UserSession
  -> Maybe Cookies'
  -> IO (Html ())
getNewTransactionPageHandler h (Authenticated sess) mbCookies = do
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
postNewTransactionHandler h (Authenticated _) formData = do
  _ <- addTransaction h $
    AddTransactionParam
      (createtransactionformSubject formData)
      (( fromIntegral (createtransactionformAmountPri formData)
       , fromMaybe (0::MoneyAmount)
                   (fromIntegral <$> createtransactionformAmountSub formData)
       ))
      (createtransactionformDay formData)
  pure . addHeader "/list"
       $ NoContent
postNewTransactionHandler _ _ _ = E.throwIO err401

-- /list

getTransactionsListPageHandler
  :: Handle
  -> AuthResult UserSession
  -> IO (Html ())
getTransactionsListPageHandler h (Authenticated sess)
  = renderTransactionsListPage sess <$> getAllTransactions h
getTransactionsListPageHandler _ _
  = E.throwIO err401

-- /view/:id

getViewTransactionByIdPageHandler
  :: Handle
  -> AuthResult UserSession
  -> GenericId
  -> IO (Html ())
getViewTransactionByIdPageHandler h (Authenticated sess) txid
  = pure $ renderViewTransactionByIdPage txid
getViewTransactionByIdPageHandler _ _ _
  = E.throwIO err401

-- /edit/:id

getEditTransactionByIdPageHandler
  :: Handle
  -> AuthResult UserSession
  -> Maybe Cookies'
  -> GenericId
  -> IO (Html ())
getEditTransactionByIdPageHandler h (Authenticated sess) mbCookies txid
  = pure . flip renderEditTransactionByIdPage txid
  $ join (lookupXsrfTokenText (cookieXsrfSetting (h^.cookieSettings)) <$>
                              mbCookies)
getEditTransactionByIdPageHandler _ _ _ _
  = E.throwIO err401

postEditTransactionHandler
  :: Handle
  -> AuthResult UserSession
  -> EditTransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postEditTransactionHandler h (Authenticated _) formData = do
  pure . addHeader "/list"
       $ NoContent
postEditTransactionHandler _ _ _ = E.throwIO err401

------------------------------------------------------------------------

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
  :<|> getViewTransactionByIdPageHandler h
  :<|> getEditTransactionByIdPageHandler h
  :<|> postEditTransactionHandler h

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
convert
  = Handler
  . ExceptT
  . E.try

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
appWith h = serveWithContext api (authContextVal h) (server h)

----------------------------------------------------------------------------

run :: IO ()
run = withHandle $ \h -> runWith (appWith h)

runWith :: Application -> IO ()
runWith
  = Warp.runTLS tlsSettings warpSettings
  . Gzip.gzip Gzip.def
  . ForceSSL.forceSSL
  . Hsts.hsts
  . VerifyCsrToken.cookieToHeader
  . VerifyCsrToken.doubleSubmitCookie

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
