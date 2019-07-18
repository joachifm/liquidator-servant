{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.User where

import Liquidator.SchemaTH
import Liquidator.Schema.Types

------------------------------------------------------------------------

data User = User
  { user_id :: Int64
  , user_first_name :: Text
  , user_last_name :: Text
  , user_email :: Text
  , user_companies :: [Int64]
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary User where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''User)

instance ToSchema User where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data UserCreate = UserCreate
  { userCreate_first_name :: Text
  , userCreate_last_name :: Text
  , userCreate_email :: Text
  , userCreate_password :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance Arbitrary UserCreate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''UserCreate)

instance ToSchema UserCreate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------

data UserUpdate = UserUpdate
  { userUpdate_first_name :: Text
  , userUpdate_last_name :: Text
  , userUpdate_email :: Text
  , userUpdate_password :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary UserUpdate where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''UserUpdate)

instance ToSchema UserUpdate where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
