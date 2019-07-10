{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Control.Lens hiding (Strict)
import qualified Data.Aeson as Aeson
import Control.Monad.Except (MonadError(..))

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

getTransactionById :: (Monad m, MonadError ServantErr m) => Int64 -> Int64 -> m Transaction
getTransactionById _ _ = throwError err404

type AddTransaction
  = "transaction"
  :> ReqBody '[JSON] Transaction
  :> Post '[JSON] Transaction

addTransaction :: (Monad m, MonadError ServantErr m) => Transaction -> m Transaction
addTransaction _ = throwError err404

type UpdateTransaction
  = "transaction"
  :> ReqBody '[JSON] Transaction
  :> Put '[JSON] Transaction

updateTransaction :: (Monad m, MonadError ServantErr m) => Transaction -> m Transaction
updateTransaction _ = throwError err404

type DeleteTransaction
  = "transaction"
  :> QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Delete '[JSON] NoContent

deleteTransaction :: (Monad m, MonadError ServantErr m) => Int64 -> Int64 -> m NoContent
deleteTransaction _ _ = throwError err404

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

server :: Server Api
server = return swaggerDoc
  :<|> getTransactionById
  :<|> addTransaction
  :<|> updateTransaction
  :<|> deleteTransaction

app :: Application
app = serve api server
