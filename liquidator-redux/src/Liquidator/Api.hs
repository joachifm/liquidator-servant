{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Liquidator.Api where

import Imports

import Lens.Micro.Platform

import Data.Swagger
import Data.Aeson (FromJSON, ToJSON)

import Servant
import Servant.Swagger

------------------------------------------------------------------------
-- Liquidator toplevel API
------------------------------------------------------------------------

type LiquidatorApi = "api" :> "v1.0" :> EmptyAPI

liquidatorApi :: Proxy LiquidatorApi
liquidatorApi = Proxy

apiVersion :: Text
apiVersion = "1.0.0"

------------------------------------------------------------------------
-- Swagger
------------------------------------------------------------------------

type SwaggerApi = "swagger.json" :> Get '[JSON] Swagger

swaggerDoc :: Swagger
swaggerDoc = toSwagger liquidatorApi
  & info.title       .~ "liquidator"
  & info.version     .~ apiVersion
  & info.description ?~ "liquidator API"
  & info.license     ?~ ("AGPL-3.0" & url ?~ URL "https://opensource.org/licenses/AGPL-3.0")
