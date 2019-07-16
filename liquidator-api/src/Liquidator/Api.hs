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
     )

liquidatorApi :: Proxy LiquidatorApi
liquidatorApi = Proxy

------------------------------------------------------------------------
-- Swagger
------------------------------------------------------------------------

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

apiVersion :: Text
apiVersion = "1.0"

swaggerDoc :: Swagger
swaggerDoc = toSwagger liquidatorApi
  & info.title       .~ "liquidator"
  & info.version     .~ apiVersion
  & info.description ?~ "liquidator API"
  & info.license     ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")

------------------------------------------------------------------------
-- Toplevel
------------------------------------------------------------------------

type Api = SwaggerApi :<|> LiquidatorApi

api :: Proxy Api
api = Proxy
