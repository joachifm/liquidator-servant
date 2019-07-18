{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.Balance where

import Liquidator.SchemaTH
import Liquidator.Schema.Types

------------------------------------------------------------------------

data Balance = Balance
  { balance_company_id :: Int64
  , balance_date :: Text
  , balance_money :: Int64
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary Balance where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''Balance)

instance ToSchema Balance where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
