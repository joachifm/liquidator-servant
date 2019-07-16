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

type TransactionApi
  = "transaction"
  :> (      GetTransactionById
       :<|> AddTransaction
       :<|> UpdateTransaction
       :<|> DeleteTransaction
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
