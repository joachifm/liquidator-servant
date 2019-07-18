{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.Company where

import Liquidator.SchemaTH
import Liquidator.Schema.Types
import Liquidator.Schema.UserCompanyRelation

------------------------------------------------------------------------

data Company = Company
  { company_id :: Int64
  , company_name :: Text
  , company_org_nr :: Text
  , company_users :: [UserCompanyRelation]
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary Company where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''Company)

instance ToSchema Company where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data CompanyCreate = CompanyCreate
  { companyCreate_name :: Text
  , companyCreate_org_nr :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary CompanyCreate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''CompanyCreate)

instance ToSchema CompanyCreate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data CompanyUpdate = CompanyUpdate
  { companyUpdate_id :: Int64
  , companyUpdate_name :: Text
  , companyUpdate_org_nr :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary CompanyUpdate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''CompanyUpdate)

instance ToSchema CompanyUpdate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
