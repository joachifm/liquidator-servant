{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Schema.UserKeys where

import Liquidator.SchemaTH
import Liquidator.Schema.Types
import Liquidator.Schema.User

------------------------------------------------------------------------

data UserKeys = UserKeys
  { userKeys_refresh :: Text
  , userKeys_access :: Text
  , userKeys_user :: User
  }
  deriving (Generic, Typeable, Eq, Show)

instance Arbitrary UserKeys where
  arbitrary = genericArbitrary
  shrink = genericShrink

$(deriveJSON ''UserKeys)

instance ToSchema UserKeys where
  declareNamedSchema = genericDeclareNamedSchema schemaOptions
