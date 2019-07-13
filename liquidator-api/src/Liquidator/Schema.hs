{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE StandaloneDeriving #-}

module Liquidator.Schema
  ( -- * Re-exports
    Int64
  , Text

    -- * Schema types
  , Url
  , Pagination(..)

  , Role(..)

  , Company(..)

  , User(..)
  , UserCreate(..)

  , TransactionType(..)
  , Transaction(..)
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger

import Servant.Swagger

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = Char.toLower x : xs

dropLabelPrefix :: String -> String -> String
dropLabelPrefix labelPrefix = lowerFirst . drop (length labelPrefix)

------------------------------------------------------------------------
-- Orphan Arbitrary instances
------------------------------------------------------------------------

instance QC.Arbitrary Text where
  arbitrary = QC.oneof [ pure mempty, pure (Text.pack "<placeholder>") ]

------------------------------------------------------------------------
-- Role
------------------------------------------------------------------------

data Role = ReporterRole | UserRole | OwnerRole
  deriving (Eq, Generic, Typeable)

instance QC.Arbitrary Role where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

instance FromJSON Role
instance ToJSON Role where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToSchema Role

------------------------------------------------------------------------
-- Pagination
------------------------------------------------------------------------

type Url = Text

data Pagination = Pagination
  { paginationPage :: Int64
  , paginationNext :: Url
  , paginationPrevious :: Url
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary Pagination where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

-- TODO(joachifm) make this generic somehow (?)
paginationJsonOptions :: Aeson.Options
paginationJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropLabelPrefix "pagination"
  }

instance FromJSON Pagination where
  parseJSON = Aeson.genericParseJSON paginationJsonOptions

instance ToJSON Pagination where
  toEncoding = Aeson.genericToEncoding paginationJsonOptions
  toJSON = Aeson.genericToJSON paginationJsonOptions

instance ToSchema Pagination where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions paginationJsonOptions)

------------------------------------------------------------------------
-- User
------------------------------------------------------------------------

data User = User
  { userId :: Int64
  , userFirstName :: Text
  , userLastName :: Text
  , userEmail :: Text
  , userCompanies :: [Int64]
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary User where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

userJsonOptions :: Aeson.Options
userJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropLabelPrefix "user"
  }

instance FromJSON User where
  parseJSON = Aeson.genericParseJSON userJsonOptions

instance ToJSON User where
  toEncoding = Aeson.genericToEncoding userJsonOptions
  toJSON = Aeson.genericToJSON userJsonOptions

instance ToSchema User where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions userJsonOptions)

------------------------------------------------------------------------
-- UserCreate
------------------------------------------------------------------------

data UserCreate = UserCreate
  { userCreateFirstName :: Text
  , userCreateLastName :: Text
  , userCreateEmail :: Text
  , userCreatePassword :: Text
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary UserCreate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

userCreateJsonOptions :: Aeson.Options
userCreateJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropLabelPrefix "userCreate"
  }

instance FromJSON UserCreate where
  parseJSON = Aeson.genericParseJSON userCreateJsonOptions

instance ToJSON UserCreate where
  toEncoding = Aeson.genericToEncoding userCreateJsonOptions
  toJSON = Aeson.genericToJSON userCreateJsonOptions

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

data Company = Company
  { companyId :: Int64
  , companyName :: Text
  , companyOrgNr :: Text
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary Company where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

companyJsonOptions :: Aeson.Options
companyJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropLabelPrefix "company"
  }

instance FromJSON Company where
  parseJSON = Aeson.genericParseJSON companyJsonOptions

instance ToJSON Company where
  toEncoding = Aeson.genericToEncoding companyJsonOptions
  toJSON = Aeson.genericToJSON companyJsonOptions

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

data TransactionType
  = Income
  | Expense
  deriving (Generic, Typeable)

instance QC.Arbitrary TransactionType where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

instance ToSchema TransactionType
instance ToParamSchema TransactionType
instance FromJSON TransactionType
instance ToJSON TransactionType where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

data Transaction = Transaction
  { transactionId :: Int64
  , transactionCompanyId :: Int64
  , transactionRecurringId :: Maybe Int64
  , transactionDate :: Text
  , transactionMoney :: Int64
  , transactionType :: TransactionType
  , transactionDescription :: Text
  , transactionNotes :: Text
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary Transaction where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

deriving instance Show TransactionType
deriving instance Show Transaction

transactionJsonOptions :: Aeson.Options
transactionJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropLabelPrefix "transaction"
  }

instance FromJSON Transaction where
  parseJSON = Aeson.genericParseJSON transactionJsonOptions

instance ToJSON Transaction where
  toJSON = Aeson.genericToJSON transactionJsonOptions
  toEncoding = Aeson.genericToEncoding transactionJsonOptions

instance ToSchema Transaction where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions transactionJsonOptions)

instance Semigroup Transaction where
  _ <> r = r

------------------------------------------------------------------------
-- Balance
------------------------------------------------------------------------

data Balance = Balance
  { balanceCompanyId :: Int64
  , balanceDate :: Text
  , balanceMoney :: Int64
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary Balance where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

balanceJsonOptions :: Aeson.Options
balanceJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = dropLabelPrefix "balance"
  }

instance FromJSON Balance where
  parseJSON = Aeson.genericParseJSON balanceJsonOptions

instance ToJSON Balance where
  toJSON = Aeson.genericToJSON balanceJsonOptions
  toEncoding = Aeson.genericToEncoding balanceJsonOptions

instance ToSchema Balance where
  declareNamedSchema = genericDeclareNamedSchema (fromAesonOptions balanceJsonOptions)
