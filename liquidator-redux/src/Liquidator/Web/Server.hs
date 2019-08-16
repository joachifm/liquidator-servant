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

import Control.AutoUpdate

import Data.IORef
import qualified Data.Map.Lazy as Map
import Data.Time.Clock (UTCTime(..), getCurrentTime)

import Servant
import qualified Network.Wai.Handler.Warp as Warp

import Html
import IORef
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

currentTime :: IO (IO UTCTime)
currentTime = mkAutoUpdate defaultUpdateSettings
  { updateAction = getCurrentTime
  , updateFreq = 1000000
  }

------------------------------------------------------------------------------

data Handle = Handle
  { hConfig :: Config
  , hCurrentTime :: IO UTCTime
  , hNextId :: IO GenericId
  , hTransactions :: IORef (Map GenericId Transaction)
  , hRecurringTransactions :: IORef (Map GenericId RecurringTransaction)
  }

newHandle :: Config -> IO Handle
newHandle cfg =
  Handle
    <$> pure cfg
    <*> currentTime
    <*> (postIncIORef <$> newIORef 1)
    <*> newIORef mempty
    <*> newIORef mempty

withHandle
  :: (Handle -> IO a)
  -> IO a
withHandle = E.bracket
  (newHandle defaultConfig)
  (const $ return ())

today :: Handle -> IO Day
today h = utctDay <$> hCurrentTime h

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
  txid <- hNextId h
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
    Just _ ->
      atomicModifyIORef' (hTransactions h) $ \m ->
        (Map.delete txid m, True)
    Nothing ->
      return False

getBalanceByDateRange
  :: Handle
  -> Maybe Day
  -> Maybe Day
  -> IO BalanceSum
getBalanceByDateRange h mbStart mbEnd
  =   sumBalance
  <$> (case rangePredicate mbStart mbEnd of
         Just p  -> getFilteredTransactions h (p . transactionDay)
         Nothing -> getAllTransactions h
      )
  where
    rangePredicate (Just start) (Just end) = Just (\x -> x >= start && x <= end)
    rangePredicate (Just start) Nothing    = Just (\x -> x >= start)
    rangePredicate Nothing (Just end)      = Just (\x ->               x <= end)
    rangePredicate Nothing Nothing         = Nothing

getAllRecurringTransactions
  :: Handle
  -> IO [(GenericId, RecurringTransaction)]
getAllRecurringTransactions h
  = Map.toList <$> readIORef (hRecurringTransactions h)

getRecurringTransactionById
  :: Handle
  -> GenericId
  -> IO (Maybe RecurringTransaction)
getRecurringTransactionById h txid
  = Map.lookup txid <$> readIORef (hRecurringTransactions h)

addRecurringTransaction
  :: Handle
  -> RecurringTransaction
  -> IO GenericId
addRecurringTransaction h tx = do
  txid <- hNextId h
  atomicModifyIORef' (hRecurringTransactions h) $ \m ->
    (Map.insert txid tx m, txid)

updateRecurringTransaction
  :: Handle
  -> GenericId
  -> RecurringTransaction
  -> IO (Maybe Bool)
updateRecurringTransaction h txid tx = do
  mbExisting <- getRecurringTransactionById h txid
  case mbExisting of
    Just txOld ->
      atomicModifyIORef' (hRecurringTransactions h) $ \m ->
        let
          edited = tx /= txOld
        in
          ( if edited then Map.insert txid tx m else m
          , Just edited
          )
    Nothing ->
      return Nothing

deleteRecurringTransaction
  :: Handle
  -> GenericId
  -> IO Bool
deleteRecurringTransaction h txid = do
  mbStored <- getRecurringTransactionById h txid
  case mbStored of
    Just _ -> do
      atomicModifyIORef' (hRecurringTransactions h) $ \m ->
        (Map.delete txid m, True)
    Nothing ->
      return False

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
  -> Maybe Day
  -> IO (Html ())
getBalanceByDatePageHandler h mbStartDay mbEndDay = do
  endDay <- maybe (today h) pure mbEndDay
  BalanceSum txcount txsum <- getBalanceByDateRange h mbStartDay (Just endDay)
  pure . Views.viewBalancePage $
    Balance txsum txcount mbStartDay (Just endDay)

-- /recurring/new

getNewRecurringTransactionPageHandler
  :: Handle
  -> IO (Html ())
getNewRecurringTransactionPageHandler _ = do
  pure Views.newRecurringTransactionPage

postNewRecurringTransactionHandler
  :: Handle
  -> RecurringTransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postNewRecurringTransactionHandler h formData = do
  _ <- addRecurringTransaction h (makeRecurringTransactionFromFormData formData)
  pure . addHeader "/recurring/list"
       $ NoContent

-- /recurring/list

getRecurringTransactionsListPageHandler
  :: Handle
  -> IO (Html ())
getRecurringTransactionsListPageHandler h
  = Views.recurringTransactionsListPage <$> getAllRecurringTransactions h

-- /recurring/view/:id

getViewRecurringTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getViewRecurringTransactionByIdPageHandler _ txid
  = pure $ Views.viewRecurringTransactionByIdPage txid

-- /recurring/edit/:id

getEditRecurringTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getEditRecurringTransactionByIdPageHandler h txid
  = Views.editRecurringTransactionByIdPage txid <$> getRecurringTransactionById h txid

postEditRecurringTransactionByIdHandler
  :: Handle
  -> GenericId
  -> RecurringTransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postEditRecurringTransactionByIdHandler h txid formData = do
  _ <- updateRecurringTransaction h txid (makeRecurringTransactionFromFormData formData)
  pure . addHeader "/recurring/list"
       $ NoContent

-- /recurring/delete/:id

getDeleteRecurringTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getDeleteRecurringTransactionByIdPageHandler h txid
  = Views.deleteRecurringTransactionByIdPage txid <$> getRecurringTransactionById h txid
postDeleteRecurringTransactionByIdHandler
  :: Handle
  -> GenericId
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postDeleteRecurringTransactionByIdHandler h txid = do
  _ <- deleteRecurringTransaction h txid
  pure . addHeader "/recurring/list"
       $ NoContent

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
  :<|> getRecurringTransactionsListPageHandler h
  :<|> getNewRecurringTransactionPageHandler h
  :<|> postNewRecurringTransactionHandler h
  :<|> getViewRecurringTransactionByIdPageHandler h
  :<|> getEditRecurringTransactionByIdPageHandler h
  :<|> postEditRecurringTransactionByIdHandler h
  :<|> getDeleteRecurringTransactionByIdPageHandler h
  :<|> postDeleteRecurringTransactionByIdHandler h

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
run = withHandle $ \h -> do
  Warp.runSettings warpSettings (app h)

warpSettings :: Warp.Settings
warpSettings
  = announce
  . devSettings
  $ Warp.defaultSettings

announce :: (Warp.Settings -> Warp.Settings)
announce ws = flip Warp.setBeforeMainLoop ws $ putStrLn $
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
