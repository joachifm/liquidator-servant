{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator.Api
  ( Api
  , api

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
  =  "api"
  :> "v1"
  :> "transaction"
  :> (      GetTransactionById
       :<|> AddTransaction
       :<|> UpdateTransaction
       :<|> DeleteTransaction
     )

------------------------------------------------------------------------
-- Liquidator
------------------------------------------------------------------------

type LiquidatorApi = TransactionApi

liquidatorApi :: Proxy LiquidatorApi
liquidatorApi = Proxy

------------------------------------------------------------------------
-- Swagger
------------------------------------------------------------------------

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

swaggerDoc :: Swagger
swaggerDoc = toSwagger liquidatorApi
  & info.title       .~ "liquidator"
  & info.version     .~ "1.0"
  & info.description ?~ "liquidator API"
  & info.license     ?~ ("MIT" & url ?~ URL "https://opensource.org/licenses/MIT")

------------------------------------------------------------------------
-- Toplevel
------------------------------------------------------------------------

type Api = SwaggerApi :<|> LiquidatorApi

api :: Proxy Api
api = Proxy
