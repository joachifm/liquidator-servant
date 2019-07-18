{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.Types
  ( module X
  , Role(..)
  , TransactionType(..)
  , Url
  ) where

import Data.Int as X (Int32, Int64)
import Data.Text as X (Text)

import Data.Typeable as X (Typeable)
import GHC.Generics as X (Generic)
import qualified Data.Text as Text

import Test.QuickCheck as X (Arbitrary(..))
import Test.QuickCheck.Arbitrary.Generic as X (genericArbitrary, genericShrink)
import qualified Test.QuickCheck as QC

import Data.Swagger as X (ToSchema(..), ToParamSchema(..), genericDeclareNamedSchema)
import Web.HttpApiData as X (FromHttpApiData(..), ToHttpApiData(..))

import Liquidator.SchemaTH

------------------------------------------------------------------------
-- Orphan Arbitrary instances
------------------------------------------------------------------------

instance Arbitrary Text where
  arbitrary = QC.oneof [ pure mempty, pure (Text.pack "<placeholder>") ]

------------------------------------------------------------------------
-- Url
------------------------------------------------------------------------

type Url = Text

------------------------------------------------------------------------
-- Role
------------------------------------------------------------------------

data Role = Role_Reporter | Role_User | Role_Owner
  deriving (Eq, Generic, Typeable, Show)

instance QC.Arbitrary Role where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''Role)

instance ToSchema Role where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance ToParamSchema Role

instance FromHttpApiData Role where
  parseUrlPiece s = case lookup s tbl of
    Just v -> Right v
    Nothing -> Left ("invalid role: " <> s)
    where
      tbl = [("User", Role_User)
            ,("Reporter", Role_Reporter)
            ,("Owner", Role_Owner)
            ]

------------------------------------------------------------------------
-- Transaction type
------------------------------------------------------------------------

data TransactionType
  = TransactionType_IN
  | TransactionType_EX
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary TransactionType where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''TransactionType)

instance ToSchema TransactionType where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

instance ToParamSchema TransactionType
