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
getBalanceByDate :: Int64 -> Text -> ClientM Balance
_ :<|> (      getTransactionById
         :<|> addTransaction
         :<|> updateTransaction
         :<|> deleteTransaction
       )
  :<|> (      getBalanceByDate
       )
  :<|> (      getCompanyById
       )
  = client api
