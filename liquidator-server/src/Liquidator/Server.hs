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
  [ Transaction 0 1 Nothing "1970-01-01" 1000 TransactionType_Income "" ""
  , Transaction 0 1 Nothing "1970-01-02" 1000 TransactionType_Income "" ""
  , Transaction 0 1 Nothing "1970-01-03" 1000 TransactionType_Income "" ""
  , Transaction 0 1 Nothing "1970-01-04" 3000 TransactionType_Expense "" ""
  ]
  where
    assign txid tx = (txid, tx { transaction_id = txid })

newHandle :: IO Handle
newHandle = Handle <$> IORef.newIORef dummyDb <*> IORef.newIORef 1

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

getCompanyById :: Handle -> Int64 -> IO Company
getCompanyById ctx companyId_ = do
  -- TODO(joachifm) return actual data
  return $! Company
    { company_id = companyId_
    , company_name = "Acme"
    , company_org_nr = "12345"
    }

addCompany :: Handle -> Company -> IO Company
addCompany ctx companyData = do
  return companyData

updateCompany :: Handle -> Company -> IO Company
updateCompany ctx companyData = do
  return companyData

deleteCompany :: Handle -> Int64 -> IO NoContent
deleteCompany ctx companyId_ = do
  return NoContent

addUserToCompany :: Handle -> Int64 -> Int64 -> Maybe Role -> IO NoContent
addUserToCompany ctx companyId_ userId_ userRole_ = do
  return NoContent

removeUserFromCompany :: Handle -> Int64 -> Int64 -> IO NoContent
removeUserFromCompany ctx companyId_ userId_ = do
  return NoContent

setUserRole :: Handle -> Int64 -> Int64 -> Role -> IO NoContent
setUserRole ctx companyId_ userId_ userRole_ = do
  return NoContent

------------------------------------------------------------------------
-- Recurring transaction
------------------------------------------------------------------------

getRecurringTransaction
  :: Handle
  -> Int64
  -> Int64
  -> IO RecurringTransaction
getRecurringTransaction _ _ _ = throwIO err404

addRecurringTransaction
  :: Handle
  -> RecurringTransaction
  -> IO RecurringTransaction
addRecurringTransaction _ _ = throwIO err404

updateRecurringTransaction
  :: Handle
  -> RecurringTransaction
  -> IO RecurringTransaction
updateRecurringTransaction _ _ = throwIO err404

deleteRecurringTransaction
  :: Handle
  -> Int64
  -> Int64
  -> IO NoContent
deleteRecurringTransaction _ _ _ = throwIO err404

getAllRecurringTransactions
  :: Handle
  -> Int64
  -> IO [RecurringTransaction]
getAllRecurringTransactions _ _ = throwIO err404

getActiveRecurringTransactions
  :: Handle
  -> Int64
  -> IO [RecurringTransaction]
getActiveRecurringTransactions _ _ = throwIO err404

getRecurringTransactionsByDate
  :: Handle
  -> Int64
  -> Text
  -> IO [RecurringTransaction]
getRecurringTransactionsByDate _ _ _ = throwIO err404

getRecurringTransactionsByDateRange
  :: Handle
  -> Int64
  -> Text
  -> Text
  -> IO [RecurringTransaction]
getRecurringTransactionsByDateRange _ _ _ _ = throwIO err404

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

getTransactionById :: Handle -> Int64 -> Int64 -> IO Transaction
getTransactionById ctx txid companyId_ = do
  db <- IORef.readIORef (transactionDb ctx)
  case Map.lookup txid (Map.filter ((== companyId_) . transaction_company_id) db) of
    Nothing  -> throwIO err404
    Just elt -> return elt

addTransaction :: Handle -> Transaction -> IO Transaction
addTransaction ctx tx = do
  txid <- IORef.atomicModifyIORef' (nextId ctx) (\s -> (s + 1, s))
  let tx' = tx { transaction_id = txid }
  -- TODO(joachifm) silently overwrites on duplicate pkey
  IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    (Map.insert txid tx s, ())
  return tx'

updateTransaction :: Handle -> Transaction -> IO Transaction
updateTransaction ctx tx = do
  mbTx <- IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    let
      txid = transaction_id tx
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
    case Map.lookup txid (Map.filter ((== companyId_) . transaction_company_id) s) of
      Just _  -> (Map.delete txid s, True)
      Nothing -> (s, False)
  if deleted
    then return NoContent
    else throwIO err404

insertTransactionArray :: Handle -> [Transaction] -> IO [Transaction]
insertTransactionArray ctx = mapM (addTransaction ctx)

getAllTransaction :: Handle -> Int64 -> IO [Transaction]
getAllTransaction _ _ = return []

getTransactionsByDate :: Handle -> Int64 -> Text -> IO [Transaction]
getTransactionsByDate _ _ _ = return []

getTransactionsByDateRange :: Handle -> Int64 -> Text -> Text -> IO [Transaction]
getTransactionsByDateRange _ _ _ _ = return []

getAllIncomeTransactions :: Handle -> Int64 -> IO [Transaction]
getAllIncomeTransactions _ _ = return []

getAllExpenseTransactions :: Handle -> Int64 -> IO [Transaction]
getAllExpenseTransactions _ _ = return []

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
      (case transaction_type tx of
         TransactionType_Income -> id
         TransactionType_Expense -> negate)
      (transaction_money tx)

    transactionByDatePredicate :: (Transaction -> Bool)
    transactionByDatePredicate p =
         ((== companyId_) . transaction_company_id) p
      && ((<= date) . transaction_date) p

getBalanceByDate
  :: Handle
  -> Int64
  -> Text
  -> IO Balance
getBalanceByDate ctx companyId_ date =
  Balance <$> pure companyId_
          <*> pure date
          <*> balanceMoneyByDate companyId_ date <$> IORef.readIORef (transactionDb ctx)

getBalanceByDateRange
  :: Handle
  -> Int64
  -> Text
  -> Text
  -> IO [Balance]
getBalanceByDateRange _ _ _ _ = throwIO err404

getBankBalance
  :: Handle
  -> Int64
  -> Int64
  -> IO BankBalance
getBankBalance _ _ _ = throwIO err404

createBankBalance
  :: Handle
  -> BankBalance
  -> IO BankBalance
createBankBalance _ _ = throwIO err404

updateBankBalance
  :: Handle
  -> BankBalance
  -> IO BankBalance
updateBankBalance _ bal = return bal

deleteBankBalance
  :: Handle
  -> Int64
  -> Int64
  -> IO NoContent
deleteBankBalance _ _ _ = return NoContent

getBankBalanceByDate
  :: Handle
  -> Int64
  -> Text
  -> IO [BankBalance]
getBankBalanceByDate _ _ _ = return []

getBankBalanceByDateRange
  :: Handle
  -> Int64
  -> Text
  -> Text
  -> IO [BankBalance]
getBankBalanceByDateRange _ _ _ _ = return []

------------------------------------------------------------------------
-- User
------------------------------------------------------------------------

getUserById
  :: Handle
  -> Int64
  -> IO User
getUserById _ _ = throwIO err404

createUser
  :: Handle
  -> UserCreate
  -> IO User
createUser _ _ = throwIO err404

updateUser
  :: Handle
  -> User
  -> IO User
updateUser _ _ = throwIO err404

deleteUser
  :: Handle
  -> Text
  -> IO NoContent
deleteUser _ _ = throwIO err404

getUserByEmail
  :: Handle
  -> Text
  -> IO User
getUserByEmail _ _ = throwIO err404

loginUser
  :: Handle
  -> LoginData
  -> IO LoginSuccess
loginUser _ _ = throwIO err404

refreshToken
  :: Handle
  -> Refresh
  -> IO RefreshResult
refreshToken _ _ = throwIO err404

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
         :<|> insertTransactionArray ctx
         :<|> getAllTransaction ctx
         :<|> getTransactionsByDate ctx
         :<|> getTransactionsByDateRange ctx
         :<|> getAllIncomeTransactions ctx
         :<|> getAllExpenseTransactions ctx
       )
  :<|> (      getBalanceByDate ctx
         :<|> getBalanceByDateRange ctx
         :<|> (     getBankBalance ctx
               :<|> createBankBalance ctx
               :<|> updateBankBalance ctx
               :<|> deleteBankBalance ctx
               :<|> getBankBalanceByDate ctx
               :<|> getBankBalanceByDateRange ctx
              )
       )
  :<|> (      getCompanyById ctx
         :<|> addCompany ctx
         :<|> updateCompany ctx
         :<|> deleteCompany ctx
         :<|> addUserToCompany ctx
         :<|> removeUserFromCompany ctx
         :<|> setUserRole ctx
       )
  :<|> (      getRecurringTransaction ctx
         :<|> addRecurringTransaction ctx
         :<|> updateRecurringTransaction ctx
         :<|> deleteRecurringTransaction ctx
         :<|> getAllRecurringTransactions ctx
         :<|> getActiveRecurringTransactions ctx
         :<|> getRecurringTransactionsByDate ctx
         :<|> getRecurringTransactionsByDateRange ctx
       )
  :<|> (      getUserById ctx
         :<|> createUser ctx
         :<|> updateUser ctx
         :<|> deleteUser ctx
         :<|> getUserByEmail ctx
         :<|> loginUser ctx
         :<|> refreshToken ctx
       )

------------------------------------------------------------------------
-- Servant application
------------------------------------------------------------------------

-- | A natural transformation from our preferred handler context
-- to the one expected by servant.
nt :: IO a -> Handler a
nt = Handler . ExceptT . (try :: IO a -> IO (Either ServerError a))

server :: Handle -> Server Api
server = hoistServer api nt . server'

app :: IO Application
app = serve api . server <$> newHandle
