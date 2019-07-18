{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.UserCompanyRelation where

import Liquidator.SchemaTH
import Liquidator.Schema.Types

------------------------------------------------------------------------

data UserCompanyRelation = UserCompanyRelation
  { userCompanyRelation_company_id :: Int64
  , userCompanyRelation_user_id :: Int64
  , userCompanyRelation_role :: Role
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary UserCompanyRelation where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''UserCompanyRelation)

instance ToSchema UserCompanyRelation where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
