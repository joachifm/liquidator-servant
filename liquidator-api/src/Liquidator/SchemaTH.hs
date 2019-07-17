{-# LANGUAGE TemplateHaskell #-}

module Liquidator.SchemaTH
  ( deriveJSON
  , aesonOptions
  , schemaOptions
  ) where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Data.List as List
import qualified Data.Char as Char

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import qualified Data.Swagger as Swagger

-- >>> nameModifier "labelPrefix_json_name"
-- "json_name"
nameModifier :: String -> String
nameModifier = map Char.toLower . List.drop 1 . List.dropWhile (/= '_')

aesonOptions :: Aeson.Options
aesonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier     = nameModifier
  , Aeson.constructorTagModifier = nameModifier
  }

deriveJSON :: TH.Name -> TH.Q [TH.Dec]
deriveJSON = Aeson.deriveJSON aesonOptions

schemaOptions :: Swagger.SchemaOptions
schemaOptions = Swagger.fromAesonOptions aesonOptions
