{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  , api
  , app
  , swaggerDoc
  ) where

import Control.Exception (throwIO, try)
import Control.Lens hiding (Strict)
import qualified Data.Aeson as Aeson
import Control.Monad.Except (ExceptT(..))

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

getTransactionById :: Int64 -> Int64 -> IO Transaction
getTransactionById _ _ = throwIO err404

type AddTransaction
  = "transaction"
  :> ReqBody '[JSON] Transaction
  :> Post '[JSON] Transaction

addTransaction :: Transaction -> IO Transaction
addTransaction _ = throwIO err404

type UpdateTransaction
  = "transaction"
  :> ReqBody '[JSON] Transaction
  :> Put '[JSON] Transaction

updateTransaction :: Transaction -> IO Transaction
updateTransaction _ = throwIO err404

type DeleteTransaction
  = "transaction"
  :> QueryParam' '[Required, Strict] "id" Int64
  :> QueryParam' '[Required, Strict] "company_id" Int64
  :> Delete '[JSON] NoContent

deleteTransaction :: Int64 -> Int64 -> IO NoContent
deleteTransaction _ _ = throwIO err404

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

------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------

-- | The underlying IO implementation of the API handler, to be hoisted into
-- the servant handler context.
server' :: ServerT Api IO
server' = return swaggerDoc
  :<|> getTransactionById
  :<|> addTransaction
  :<|> updateTransaction
  :<|> deleteTransaction

-- | A natural transformation from our preferred handler context
-- to the one expected by servant.
nt :: IO a -> Handler a
nt = Handler . ExceptT . (try :: IO a -> IO (Either ServantErr a))

server :: Server Api
server = hoistServer api nt server'

app :: Application
app = serve api server
