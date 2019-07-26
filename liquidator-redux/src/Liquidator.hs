{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Liquidator where

import GHC.Generics (Generic)

import Control.Lens.Operators
import Control.Lens.TH

import qualified Control.Exception as E
import Control.Monad.Except (ExceptT(ExceptT))

import Data.Int (Int32, Int64)
import Data.Word (Word32)

import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Data.Map (Map)
import qualified Data.Map.Lazy as Map

import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as Aeson

import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

import Data.Time.Clock.System (SystemTime(..), getSystemTime)

import Lucid (Html, toHtml)
import Lucid.Html5
import Servant.HTML.Lucid

import Servant
import Servant.Auth
import Servant.Auth.Server

import Crypto.JOSE.JWK (JWK)

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

import qualified Network.Wai.Middleware.AddHeaders as AddHeaders
import qualified Network.Wai.Middleware.ForceSSL as ForceSSL
import qualified Network.Wai.Middleware.Gzip as Gzip

import Money

------------------------------------------------------------------------

postIncIORef :: (Integral a) => IORef a -> IO a
postIncIORef = flip atomicModifyIORef' (\i -> (i + 1, i))

atomicModifyIORef'_
  :: IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef'_ ref act = atomicModifyIORef' ref ((,()) . act)

----------------------------------------------------------------------------

data Role
  = RoleUser
  | RoleOwner
  | RoleAdmin
  -- TODO(joachifm) derive Show et al only for devel/debug
  deriving (Eq, Read, Show, Generic, FromJSON, ToJSON)

----------------------------------------------------------------------------

type GenericId = Int64

----------------------------------------------------------------------------

type UserId = GenericId

-- | User session identifier, passed along as a JWT.  The actual information
-- is retrieved as-needed using this index to minimise the JWT payload.
data User = MkUser { userId :: UserId }
  deriving (Eq, Read, Show, Generic, FromJSON, ToJSON, FromJWT, ToJWT)

----------------------------------------------------------------------------

type TransactionId = GenericId

data Transaction = MkTransaction
  { transactionMoney :: Money
    -- ^ Transaction money to/from account.
    --
    -- Outgoing amounts are negative; incoming amounts positive.

  , transactionDate :: Text
    -- ^ Date when the transaction occurred.

  , transactionCleared :: Bool
    -- ^ Flag to indicate that the transaction has been "cleared".
    --
    -- Cleared transactions are immutable.

  , transactionClearedTime :: Maybe Text
    -- ^ Transaction clearing time.

  , transactionFlagged :: Bool
    -- ^ Flag to indicate that the transaction has been "flagged" (e.g., for
    -- review).

  , transactionNotes :: [(Int64, Text)]
    -- ^ Free-form, timestamped, transaction notes.
  }
  -- TODO(joachifm) derive Show only for debug/devel builds (?)
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

emptyTransaction :: Transaction
emptyTransaction = MkTransaction
  { transactionMoney = moneyFromAmount 0
  , transactionDate = "1970-01-01"
  , transactionCleared = False
  , transactionClearedTime = Nothing
  , transactionFlagged = False
  , transactionNotes = []
  }

----------------------------------------------------------------------------

-- | A transaction diff represents possible updates to a 'Transaction'.
--
-- See 'applyTransactionDiff'.
data TransactionDiff = TransactionDiff
  { transactionDiffSetCleared :: Maybe Bool
  , transactionDiffSetFlagged :: Maybe Bool
  , transactionDiffAddNote :: Maybe Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

emptyTransactionDiff :: TransactionDiff
emptyTransactionDiff = TransactionDiff Nothing Nothing Nothing

type TransactionEditor = (Transaction -> Transaction)

setTransactionCleared :: TransactionEditor
setTransactionCleared = \tx -> tx { transactionCleared = True }

setTransactionFlagged :: TransactionEditor
setTransactionFlagged = \tx -> tx { transactionFlagged = not (transactionFlagged tx) }

addTransactionNote :: Int64 -> Text -> TransactionEditor
addTransactionNote now text = \tx -> tx { transactionNotes = (now, text) : transactionNotes tx }

applyTransactionDiff
  :: Int64
  -> TransactionDiff
  -> TransactionEditor
applyTransactionDiff now txDiff
  = doEdit (const setTransactionCleared) transactionDiffSetCleared
  . doEdit (const setTransactionFlagged) transactionDiffSetFlagged
  . doEdit (addTransactionNote now)      transactionDiffAddNote
  where
    doEdit
      :: (a -> TransactionEditor)
      -> (TransactionDiff -> Maybe a)
      -> TransactionEditor
    doEdit e f = maybe id e (f txDiff)

------------------------------------------------------------------------

data HandlerEnv = HandlerEnv
  { users :: IORef (Map UserId User)
  , transactions :: IORef (Map TransactionId Transaction)
  , nextId :: IORef GenericId
  }

newHandlerEnv :: IO HandlerEnv
newHandlerEnv = HandlerEnv
  <$> newIORef mempty
  <*> newIORef mempty
  <*> newIORef 1

getNextId :: HandlerEnv -> IO GenericId
getNextId = postIncIORef . nextId

------------------------------------------------------------------------

createTransaction
  :: HandlerEnv
  -> Transaction
  -> IO TransactionId
createTransaction h tx = do
  txid <- getNextId h
  atomicModifyIORef' (transactions h) $ \m ->
    (Map.insert txid tx m, ())
  return txid

getAllTransactions
  :: HandlerEnv
  -> IO [(TransactionId, Transaction)]
getAllTransactions h = Map.toList <$> readIORef (transactions h)

getTransactionById
  :: HandlerEnv
  -> TransactionId
  -> IO (Maybe Transaction)
getTransactionById h txid = Map.lookup txid <$> readIORef (transactions h)

patchTransaction
  :: HandlerEnv
  -> TransactionId
  -> TransactionDiff
  -> IO ()
patchTransaction h txid txdiff = do
  now <- systemSeconds <$> getSystemTime
  atomicModifyIORef' (transactions h) $ \m ->
    ( Map.update (\tx -> Just (applyTransactionDiff now txdiff tx)) txid m
    , ())

------------------------------------------------------------------------

type TransactionApi
  =
       -- Retrieve all transactions
       Get '[JSON] [(TransactionId, Transaction)]

       -- Create an uncleared transaction
  :<|> ReqBody '[JSON] Transaction :>
       Post '[JSON] TransactionId

       -- Retrieve specific transaction
  :<|> Capture "id" TransactionId :>
       Get '[JSON] Transaction

       -- Partially update a transaction
  :<|> Capture "id" TransactionId :>
       ReqBody '[JSON] TransactionDiff :>
       Patch '[JSON] NoContent

------------------------------------------------------------------------

text_ :: Text -> Html ()
text_ = toHtml

------------------------------------------------------------------------

noContent :: IO a -> IO NoContent
noContent = (*> pure NoContent)

------------------------------------------------------------------------

transactionHandler
  :: HandlerEnv
  -> ServerT TransactionApi IO
transactionHandler h
  =    getTransactionsHandler
  :<|> createTransactionHandler
  :<|> getTransactionByIdHandler
  :<|> patchTransactionHandler
  where
    getTransactionsHandler = getAllTransactions h

    createTransactionHandler = createTransaction h

    getTransactionByIdHandler txid = do
      mb <- getTransactionById h txid
      case mb of
        Just tx -> return tx
        Nothing -> E.throwIO err404 -- TODO(joachifm) more informative message?

    patchTransactionHandler txid txdiff = noContent $
      patchTransaction h txid txdiff

type LiquidatorApi = "api" :> "v1" :> "transaction" :> TransactionApi

liquidatorHandler
  :: HandlerEnv
  -> ServerT LiquidatorApi IO
liquidatorHandler h
  = transactionHandler h

------------------------------------------------------------------------

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
      input_ [ name_ "passphrase"
             , type_ "password"
             , placeholder_ "Passphrase"
             , required_ "required"
             , tabindex_ "2"
             ]
    input_ [ type_ "submit"
           , value_ "Login"
             , tabindex_ "3"
           ]

getIndexPageHandler
  :: HandlerEnv
  -> IO (Html ())
getIndexPageHandler _ = pure $ renderIndexPage

getLoginPageHandler
  :: HandlerEnv
  -> IO (Html ())
getLoginPageHandler _ = pure $ renderLoginPage

postLoginHandler
  :: HandlerEnv
  -> IO NoContent
postLoginHandler h = noContent $ return ()

type WebApi
  =    Get '[HTML] (Html ())

  -- GET /login
  --
  -- Present login form, acquire user login details
  :<|> "login" :>
       Get '[HTML] (Html ())

  -- POST /login
  --
  -- Check credentials, provide JWT
  :<|> "login" :>
       Post '[PlainText] NoContent

webHandler :: HandlerEnv -> ServerT WebApi IO
webHandler h
  =    getIndexPageHandler h
  :<|> getLoginPageHandler h
  :<|> postLoginHandler h

----------------------------------------------------------------------------

type Api
  =    WebApi
  :<|> LiquidatorApi

handler :: HandlerEnv -> ServerT Api IO
handler h
  =    webHandler h
  :<|> liquidatorHandler h

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

server :: HandlerEnv -> Server Api
server = hoistServer api convert . handler

app :: IO Application
app = serve api . server <$> newHandlerEnv

----------------------------------------------------------------------------

run :: IO ()
run = Warp.runTLS tlsSettings devSettings . middle =<< app
  where
    middle
      = Gzip.gzip Gzip.def
      . ForceSSL.forceSSL
      . hsts

    -- See https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security
    --
    -- Along with rewriting all requests to https:// this is hoped to reduce the risk
    -- of malicious or acciddental SSL downgrades.
    hsts = AddHeaders.addHeaders [
      ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
      ]

    tlsSettings
      = (Warp.tlsSettings "site.crt" "site.key")
          { Warp.onInsecure = Warp.AllowInsecure -- relies on forceSSL
          }

    devSettings
      = Warp.setPort 3000
      . Warp.setHost "127.0.0.1"
      . Warp.setLogger (\r s _ -> print r >> print s >> putStrLn "")
      . Warp.setBeforeMainLoop (beforeMainLoop devSettings)
      $ Warp.defaultSettings

    beforeMainLoop ws = do
      putStrLn ("Listening on " <> show (Warp.getHost ws)
                                <> ":"
                                <> show (Warp.getPort ws))

----------------------------------------------------------------------------
