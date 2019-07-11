{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  , api
  , swaggerDoc
  , module Schema
  ) where

import Control.Lens hiding (Strict)
import qualified Data.Aeson as Aeson

import Data.Swagger
import Servant
import Servant.Swagger

import Schema

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

type GetTransactionById
  =  "transaction"
  :> QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Get '[JSON] Transaction

type AddTransaction
  = "transaction"
  :> ReqBody '[JSON] Transaction
  :> Post '[JSON] Transaction

type UpdateTransaction
  = "transaction"
  :> ReqBody '[JSON] Transaction
  :> Put '[JSON] Transaction

type DeleteTransaction
  = "transaction"
  :> QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Delete '[JSON] NoContent

type TransactionApi
  =    GetTransactionById
  :<|> AddTransaction
  :<|> UpdateTransaction
  :<|> DeleteTransaction

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