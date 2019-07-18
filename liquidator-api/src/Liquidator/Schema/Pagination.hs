{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.Pagination where

import Liquidator.SchemaTH
import Liquidator.Schema.Types

------------------------------------------------------------------------

data Pagination = Pagination
  { pagination_page :: Int64
  , pagination_next :: Url
  , pagination_previous :: Url
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary Pagination where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''Pagination)

instance ToSchema Pagination where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
