{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Liquidator.Web.Server
  (
    -- * Config
    Config(..)
  , defaultConfig

    -- * Handle
  , Handle
  , newHandle
  , withHandle

    -- * Servant handler
  , server

    -- * WAI app
  , app

    -- * Entrypoint
  , run
  ) where

import Imports

import Control.Monad.Except (ExceptT(ExceptT))
import qualified Control.Exception as E

import Data.IORef
import qualified Data.Map.Lazy as Map
import Data.Time.Clock (UTCTime(..), getCurrentTime)

import Servant
import qualified Network.Wai.Handler.Warp as Warp

import IORef
import Money
import Html
import Instances ()

import Liquidator.Web.Api
import qualified Liquidator.Web.Views as Views

------------------------------------------------------------------------------

data Config = Config
  { cfgPort :: Warp.Port
  , cfgHost :: Warp.HostPreference
  }

defaultConfig :: Config
defaultConfig = Config
  { cfgPort = 3000
  , cfgHost = "127.0.0.1"
  }

------------------------------------------------------------------------------

data Handle = Handle
  { hConfig :: Config
  , hNextId :: IORef GenericId
  , hTransactions :: IORef (Map GenericId Transaction)
  }

newHandle :: Config -> IO Handle
newHandle cfg = Handle
  <$> pure cfg
  <*> newIORef 1
  <*> newIORef mempty

withHandle
  :: (Handle -> IO a)
  -> IO a
withHandle act = E.bracket (newHandle defaultConfig) (const $ return ()) act

getNextId :: Handle -> IO GenericId
getNextId = postIncIORef . hNextId

today :: IO Day
today = utctDay <$> getCurrentTime

------------------------------------------------------------------------------

getAllTransactions
  :: Handle
  -> IO [(GenericId, Transaction)]
getAllTransactions h
  = Map.toList <$> readIORef (hTransactions h)

getFilteredTransactions
  :: Handle
  -> (Transaction -> Bool)
  -> IO [(GenericId, Transaction)]
getFilteredTransactions h p
  = Map.toList . Map.filter p <$> readIORef (hTransactions h)

getTransactionById
  :: Handle
  -> GenericId
  -> IO (Maybe Transaction)
getTransactionById h txid
  = Map.lookup txid <$> readIORef (hTransactions h)

addTransaction
  :: Handle
  -> Transaction
  -> IO GenericId
addTransaction h tx = do
  txid <- getNextId h
  atomicModifyIORef' (hTransactions h) $ \m ->
    (Map.insert txid tx m, txid)

updateTransaction
  :: Handle
  -> GenericId
  -> Transaction
  -> IO (Maybe Bool)
updateTransaction h txid tx = do
  mbExisting <- getTransactionById h txid
  case mbExisting of
    Just txOld ->
      atomicModifyIORef' (hTransactions h) $ \m ->
        let
          edited = tx /= txOld
        in
          ( if edited then Map.insert txid tx m else m
          , Just edited
          )
    Nothing ->
      return Nothing

deleteTransaction
  :: Handle
  -> GenericId
  -> IO Bool
deleteTransaction h txid = do
  mbStored <- getTransactionById h txid
  case mbStored of
    Just _ -> do
      atomicModifyIORef' (hTransactions h) $ \m ->
        (Map.delete txid m, True)

    Nothing ->
      return False

getBalanceByDate
  :: Handle
  -> Day
  -> IO Money
getBalanceByDate h day
  = uncurry moneyFromAmount
  . sumit . map (moneyToAmounts . transactionAmount . snd)
  <$> getFilteredTransactions h (\tx -> transactionDay tx <= day)

sumit :: [(MoneyAmount, MoneyAmount)] -> (MoneyAmount, MoneyAmount)
sumit = foldl' (\(za, zb) (a, b) -> (za + a, zb + b)) (0, 0)

------------------------------------------------------------------------------

-- index

getIndexPageHandler
  :: Handle
  -> IO (Html ())
getIndexPageHandler _ = pure $ Views.indexPage

-- /new

getNewTransactionPageHandler
  :: Handle
  -> IO (Html ())
getNewTransactionPageHandler _ = do
  pure Views.newTransactionPage

postNewTransactionHandler
  :: Handle
  -> TransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postNewTransactionHandler h formData = do
  _ <- addTransaction h (makeTransactionFromFormData formData)
  pure . addHeader "/list"
       $ NoContent

-- /list

getTransactionsListPageHandler
  :: Handle
  -> IO (Html ())
getTransactionsListPageHandler h
  = Views.transactionsListPage <$> getAllTransactions h

-- /view/:id

getViewTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getViewTransactionByIdPageHandler _ txid
  = pure $ Views.viewTransactionByIdPage txid

-- /edit/:id

getEditTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getEditTransactionByIdPageHandler h txid
  = Views.editTransactionByIdPage txid <$> getTransactionById h txid

postEditTransactionByIdHandler
  :: Handle
  -> GenericId
  -> TransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postEditTransactionByIdHandler h txid formData = do
  _ <- updateTransaction h txid (makeTransactionFromFormData formData)
  pure . addHeader "/list"
       $ NoContent

-- /delete/:id

getDeleteTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getDeleteTransactionByIdPageHandler h txid
  = Views.deleteTransactionByIdPage txid <$> getTransactionById h txid
postDeleteTransactionByIdHandler
  :: Handle
  -> GenericId
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postDeleteTransactionByIdHandler h txid = do
  _ <- deleteTransaction h txid
  pure . addHeader "/list"
       $ NoContent

-- /balance

getBalanceByDatePageHandler
  :: Handle
  -> Maybe Day
  -> IO (Html ())
getBalanceByDatePageHandler h mbDay = do
  day <- maybe today pure mbDay
  Views.viewBalanceByDatePage day <$> getBalanceByDate h day

------------------------------------------------------------------------

apiHandler
  :: Handle
  -> ServerT Api IO
apiHandler h
  =    getIndexPageHandler h
  :<|> getTransactionsListPageHandler h
  :<|> getNewTransactionPageHandler h
  :<|> postNewTransactionHandler h
  :<|> getViewTransactionByIdPageHandler h
  :<|> getEditTransactionByIdPageHandler h
  :<|> postEditTransactionByIdHandler h
  :<|> getDeleteTransactionByIdPageHandler h
  :<|> postDeleteTransactionByIdHandler h
  :<|> getBalanceByDatePageHandler h

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

server :: Handle -> Server Api
server = hoistServer api convert . apiHandler

app :: Handle -> Application
app = serve api . server

----------------------------------------------------------------------------

run :: IO ()
run = withHandle $ Warp.runSettings warpSettings . app

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
