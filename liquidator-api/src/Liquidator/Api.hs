{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator.Api
  ( Api
  , api
  , apiVersion

  , LiquidatorApi
  , liquidatorApi
  , swaggerDoc

  , module Liquidator.Schema
  ) where

import Control.Lens hiding (Strict)

import Data.Swagger
import Servant
import Servant.Swagger

import Liquidator.Schema

------------------------------------------------------------------------
-- Recurring
------------------------------------------------------------------------

type GetRecurringTransaction
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "id" Int64
  :> Get '[JSON] RecurringTransaction

type AddRecurringTransaction
  =  ReqBody '[JSON] RecurringTransaction
  :> Post '[JSON] RecurringTransaction

type UpdateRecurringTransaction
  =  ReqBody '[JSON] RecurringTransaction
  :> Put '[JSON] RecurringTransaction

type DeleteRecurringTransaction
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "id" Int64
  :> Delete '[JSON] NoContent

-- TODO(joachifm) result should be pagination
type GetAllRecurringTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [RecurringTransaction]

-- TODO(joachifm) result should be pagination
type GetActiveRecurringTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [RecurringTransaction]

-- TODO(joachifm) result should be pagination
type GetRecurringTransactionsByDate
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "date_param" Text
  :> Get '[JSON] [RecurringTransaction]

-- TODO(joachifm) result should be pagination
type GetRecurringTransactionsByDateRange
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "start_date" Text
  :> QueryParam' '[Required, Strict] "end_date" Text
  :> Get '[JSON] [RecurringTransaction]

type RecurringApi
  =  "recurring"
  :> (    GetRecurringTransaction
     :<|> AddRecurringTransaction
     :<|> UpdateRecurringTransaction
     :<|> DeleteRecurringTransaction
     :<|> ( "all" :> GetAllRecurringTransactions )
     :<|> ( "active" :> GetActiveRecurringTransactions )
     :<|> ( "byDate" :> GetRecurringTransactionsByDate )
     :<|> ( "byDateRange" :> GetRecurringTransactionsByDateRange )
     )

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

type GetCompanyById
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] Company

type CompanyApi
  =  "company"
  :> (    GetCompanyById
     )

------------------------------------------------------------------------
-- Balance
------------------------------------------------------------------------

type GetBalanceByDate
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "date" Text
  :> Get '[JSON] Balance

type BalanceApi
  = "balance"
  :> (      GetBalanceByDate
     )

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

type GetTransactionById
  =  QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] Transaction

type AddTransaction
  =  ReqBody '[JSON] Transaction
  :> Post '[JSON] Transaction

type UpdateTransaction
  =  ReqBody '[JSON] Transaction
  :> Put '[JSON] Transaction

type DeleteTransaction
  =  QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Delete '[JSON] NoContent

type InsertTransactionArray
  =  ReqBody '[JSON] [Transaction]
  :> Post '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetAllTransaction
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetTransactionsByDate
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "date_param" Text
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetTransactionsByDateRange
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> QueryParam' '[Required, Strict] "start_date" Text
  :> QueryParam' '[Required, Strict] "end_date" Text
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetAllIncomeTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [Transaction]

-- TODO(joachifm) returns transaction pagination
type GetAllExpenseTransactions
  =  QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] [Transaction]

type TransactionApi
  = "transaction"
  :> (      GetTransactionById
       :<|> AddTransaction
       :<|> UpdateTransaction
       :<|> DeleteTransaction
       :<|> ( "insertArray" :> InsertTransactionArray )
       :<|> ( "all" :> GetAllTransaction )
       :<|> ( "byDate" :> GetTransactionsByDate )
       :<|> ( "byDateRange" :> GetTransactionsByDateRange )
       :<|> ( "income" :> "all" :> GetAllIncomeTransactions )
       :<|> ( "expense" :> "all" :> GetAllExpenseTransactions )
     )

------------------------------------------------------------------------
-- Liquidator
------------------------------------------------------------------------

type LiquidatorApi
  = "api"
  :> "v1"
  :> (     TransactionApi
      :<|> BalanceApi
      :<|> CompanyApi
      :<|> RecurringApi
     )

liquidatorApi :: Proxy LiquidatorApi
liquidatorApi = Proxy

------------------------------------------------------------------------
-- Swagger
------------------------------------------------------------------------

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

apiVersion :: Text
apiVersion = "1.0.0"

swaggerDoc :: Swagger
swaggerDoc = toSwagger liquidatorApi
  & info.title       .~ "liquidator"
  & info.version     .~ apiVersion
  & info.description ?~ "liquidator API"
  & info.license     ?~ ("AGPL-3.0" & url ?~ URL "https://opensource.org/licenses/AGPL-3.0")

------------------------------------------------------------------------
-- Toplevel
------------------------------------------------------------------------

type Api = SwaggerApi :<|> LiquidatorApi

api :: Proxy Api
api = Proxy
