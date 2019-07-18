{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Liquidator.Schema
  ( module X
  , LoginData(..)
  , LoginSuccess(..)
  , Refresh(..)
  , RefreshResult(..)
  ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC

import Data.Swagger (ToSchema(..))
import qualified Data.Swagger as Swagger

import Liquidator.SchemaTH

import Liquidator.Schema.Types as X
import Liquidator.Schema.Transaction as X
import Liquidator.Schema.TransactionTemplate as X
import Liquidator.Schema.Pagination as X
import Liquidator.Schema.User as X
import Liquidator.Schema.UserCompanyRelation as X
import Liquidator.Schema.UserKeys as X
import Liquidator.Schema.Company as X
import Liquidator.Schema.RecurringTransaction as X
import Liquidator.Schema.Balance as X
import Liquidator.Schema.BankBalance as X
import Liquidator.Schema.Month as X

------------------------------------------------------------------------
-- User login
------------------------------------------------------------------------

data LoginData = LoginData
  { loginData_email :: Text
  , loginData_password :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

data LoginSuccess = LoginSuccess
  { loginSuccess_refresh :: Text
  , loginSuccess_access :: Text
  , loginSuccess_user :: User
  }
  deriving (Eq, Show, Generic, Typeable)

data Refresh = Refresh
  { refresh_refresh :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

data RefreshResult = RefreshResult
  { refresh_access :: Text
  }
  deriving (Eq, Show, Generic, Typeable)

instance QC.Arbitrary LoginData where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

instance QC.Arbitrary LoginSuccess where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

instance QC.Arbitrary Refresh where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

instance QC.Arbitrary RefreshResult where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''LoginData)
$(deriveJSON ''LoginSuccess)
$(deriveJSON ''Refresh)
$(deriveJSON ''RefreshResult)

instance ToSchema LoginData where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

instance ToSchema LoginSuccess where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

instance ToSchema Refresh where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

instance ToSchema RefreshResult where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions
