{-# LANGUAGE OverloadedStrings #-}

module Liquidator.Server
  ( app
  ) where

import Control.Exception (throwIO, try)
import Control.Monad.Except (ExceptT(..))
import Data.IORef (IORef)
import Data.Map.Strict (Map)
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map

import Servant
import Servant.Server

import Liquidator.Api

------------------------------------------------------------------------
-- App environment
------------------------------------------------------------------------

-- TODO(joachifm) IORef not thread safe, use MVar(?)

-- | An opaque resource handle.
data Handle = Handle
  { transactionDb :: !(IORef (Map Int64 Transaction))
  , nextId :: !(IORef Int64)
  }

dummyDb :: Map Int64 Transaction
dummyDb = Map.fromList . zipWith assign [ 1 .. ] $
  [ Transaction 0 1 Nothing "1970-01-01" 1000 Income "" ""
  , Transaction 0 1 Nothing "1970-01-02" 1000 Income "" ""
  , Transaction 0 1 Nothing "1970-01-03" 1000 Income "" ""
  , Transaction 0 1 Nothing "1970-01-04" 3000 Expense "" ""
  ]
  where
    assign txid tx = (txid, tx { transactionId = txid })

newHandle :: IO Handle
newHandle = Handle <$> IORef.newIORef dummyDb <*> IORef.newIORef 1

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

getCompanyById :: Handle -> Int64 -> IO Company
getCompanyById ctx companyId_ = do
  -- TODO(joachifm) return actual data
  return $! Company
    { companyId = companyId_
    , companyName = "Acme"
    , companyOrgNr = "12345"
    }

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

getTransactionById :: Handle -> Int64 -> Int64 -> IO Transaction
getTransactionById ctx txid companyId_ = do
  db <- IORef.readIORef (transactionDb ctx)
  case Map.lookup txid (Map.filter ((== companyId_) . transactionCompanyId) db) of
    Nothing  -> throwIO err404
    Just elt -> return elt

addTransaction :: Handle -> Transaction -> IO Transaction
addTransaction ctx tx = do
  txid <- IORef.atomicModifyIORef' (nextId ctx) (\s -> (s + 1, s))
  let tx' = tx { transactionId = txid }
  -- TODO(joachifm) silently overwrites on duplicate pkey
  IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    (Map.insert txid tx s, ())
  return tx'

updateTransaction :: Handle -> Transaction -> IO Transaction
updateTransaction ctx tx = do
  mbTx <- IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    let
      txid = transactionId tx
    in
      case Map.lookup txid s of
        Just tx0 ->
          let tx1 = tx0 <> tx -- right-biased merge
          in (Map.insert txid tx1 s, Just tx1)
        _ -> (s, Nothing)
  case mbTx of
    Just tx' -> return tx'
    Nothing  -> throwIO err404

deleteTransaction :: Handle -> Int64 -> Int64 -> IO NoContent
deleteTransaction ctx txid companyId_ = do
  deleted <- IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    case Map.lookup txid (Map.filter ((== companyId_) . transactionCompanyId) s) of
      Just _  -> (Map.delete txid s, True)
      Nothing -> (s, False)
  if deleted
    then return NoContent
    else throwIO err404

------------------------------------------------------------------------
-- Balance
------------------------------------------------------------------------

balanceMoneyByDate
  :: Int64
  -> Text
  -> Map Int64 Transaction
  -> Int64
balanceMoneyByDate companyId_ date
  = Map.foldl' (flip ((+) . transactionMoneyToInt)) 0
  . Map.filter transactionByDatePredicate
  where
    transactionMoneyToInt :: Transaction -> Int64
    transactionMoneyToInt tx =
      (case transactionType tx of
         Income -> id
         Expense -> negate)
      (transactionMoney tx)

    transactionByDatePredicate :: (Transaction -> Bool)
    transactionByDatePredicate p =
         ((== companyId_) . transactionCompanyId) p
      && ((<= date) . transactionDate) p

getBalanceByDate
  :: Handle
  -> Int64
  -> Text
  -> IO Balance
getBalanceByDate ctx companyId_ date =
  Balance <$> pure companyId_
          <*> pure date
          <*> balanceMoneyByDate companyId_ date <$> IORef.readIORef (transactionDb ctx)

------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------

-- | The underlying IO implementation of the API handler, to be hoisted into
-- the servant handler context.
server' :: Handle -> ServerT Api IO
server' ctx = return swaggerDoc
  :<|> (      getTransactionById ctx
         :<|> addTransaction ctx
         :<|> updateTransaction ctx
         :<|> deleteTransaction ctx
       )
  :<|> (      getBalanceByDate ctx
       )
  :<|> (      getCompanyById ctx
       )

-- | A natural transformation from our preferred handler context
-- to the one expected by servant.
nt :: IO a -> Handler a
nt = Handler . ExceptT . (try :: IO a -> IO (Either ServerError a))

server :: Handle -> Server Api
server ctx = hoistServer api nt (server' ctx)

app :: IO Application
app = serve api . server <$> newHandle
