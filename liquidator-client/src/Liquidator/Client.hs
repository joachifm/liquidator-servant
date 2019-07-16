{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator.Client where

import Servant
import Servant.Client

import Liquidator.Schema
import Liquidator.Api (api)

getTransactionById :: Int64 -> Int64 -> ClientM Transaction
addTransaction :: Transaction -> ClientM Transaction
updateTransaction :: Transaction -> ClientM Transaction
deleteTransaction :: Int64 -> Int64 -> ClientM NoContent
insertTransactionArray :: [Transaction] -> ClientM [Transaction]
getTransactionsByDate :: Int64 -> Text -> ClientM [Transaction]
getTransactionsByDateRange :: Int64 -> Text -> Text -> ClientM [Transaction]
getAllIncomeTransactions :: Int64 -> ClientM [Transaction]
getAllExpenseTransactions :: Int64 -> ClientM [Transaction]
getBalanceByDate :: Int64 -> Text -> ClientM Balance
getRecurringTransaction :: Int64 -> Int64 -> ClientM RecurringTransaction
addRecurringTransaction :: RecurringTransaction -> ClientM RecurringTransaction
updateRecurringTransaction :: RecurringTransaction -> ClientM RecurringTransaction
deleteRecurringTransaction :: Int64 -> Int64 -> ClientM NoContent
getAllRecurringTransactions :: Int64 -> ClientM [RecurringTransaction]
getActiveRecurringTransactions :: Int64 -> ClientM [RecurringTransaction]
getRecurringTransactionsByDate :: Int64 -> Text -> ClientM [RecurringTransaction]
getRecurringTransactionsByDateRange :: Int64 -> Text -> Text -> ClientM [RecurringTransaction]
_ :<|> (      getTransactionById
         :<|> addTransaction
         :<|> updateTransaction
         :<|> deleteTransaction
         :<|> insertTransactionArray
         :<|> getAllTransaction
         :<|> getTransactionsByDate
         :<|> getTransactionsByDateRange
         :<|> getAllIncomeTransactions
         :<|> getAllExpenseTransactions
       )
  :<|> (      getBalanceByDate
       )
  :<|> (      getCompanyById
       )
  :<|> (      getRecurringTransaction
         :<|> addRecurringTransaction
         :<|> updateRecurringTransaction
         :<|> deleteRecurringTransaction
         :<|> getAllRecurringTransactions
         :<|> getActiveRecurringTransactions
         :<|> getRecurringTransactionsByDate
         :<|> getRecurringTransactionsByDateRange
       )
  = client api
