{-# LANGUAGE DeriveGeneric #-}

module Liquidator.Schema
  ( -- * Re-exports
    Int64
  , Int32
  , Text

    -- * Schema types
  , Url
  , Pagination(..)

  , Role(..)

  , Company(..)

  , User(..)
  , UserCreate(..)

  , Balance(..)

  , TransactionType(..)
  , Transaction(..)
  , TransactionTemplate(..)

  , Month(..)
  , RecurringTransaction(..)
  ) where

import Data.Int (Int32, Int64)
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
import Data.Swagger (ToSchema(..), ToParamSchema(..))
import qualified Data.Swagger as Swagger

import Servant.Swagger

------------------------------------------------------------------------
-- Internal utilities
------------------------------------------------------------------------

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = Char.toLower x : xs

-- Note: not a general-purpose util; does not ensure that the input contains
-- the given prefix ...
dropLabelPrefix :: String -> String -> String
dropLabelPrefix labelPrefix = lowerFirst . drop (length labelPrefix)

camelCaseToSnake :: String -> String
camelCaseToSnake [] = []
camelCaseToSnake (x:xs)
  | Char.isUpper x = '_': Char.toLower x : camelCaseToSnake xs
  | otherwise      = x : camelCaseToSnake xs

fieldLabelModifier :: String -> String -> String
fieldLabelModifier prefix = camelCaseToSnake . lowerFirst . dropLabelPrefix prefix

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
  { Aeson.fieldLabelModifier = fieldLabelModifier "pagination"
  }

instance FromJSON Pagination where
  parseJSON = Aeson.genericParseJSON paginationJsonOptions

instance ToJSON Pagination where
  toEncoding = Aeson.genericToEncoding paginationJsonOptions
  toJSON = Aeson.genericToJSON paginationJsonOptions

instance ToSchema Pagination where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions paginationJsonOptions)

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
  { Aeson.fieldLabelModifier = fieldLabelModifier "user"
  }

instance FromJSON User where
  parseJSON = Aeson.genericParseJSON userJsonOptions

instance ToJSON User where
  toEncoding = Aeson.genericToEncoding userJsonOptions
  toJSON = Aeson.genericToJSON userJsonOptions

instance ToSchema User where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions userJsonOptions)

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
  { Aeson.fieldLabelModifier = fieldLabelModifier "userCreate"
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
  { Aeson.fieldLabelModifier = fieldLabelModifier "company"
  }

instance ToSchema Company

instance FromJSON Company where
  parseJSON = Aeson.genericParseJSON companyJsonOptions

instance ToJSON Company where
  toEncoding = Aeson.genericToEncoding companyJsonOptions
  toJSON = Aeson.genericToJSON companyJsonOptions

------------------------------------------------------------------------
-- Transaction template
------------------------------------------------------------------------

data TransactionTemplate = TransactionTemplate
  { transactionTemplateId :: Int64
  , transactionTemplateMoney :: Int64
  , transactionTemplateType :: TransactionType
  , transactionTemplateDescription :: Text
  , transactionTemplateNote :: Text
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary TransactionTemplate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

transactionTemplateJsonOptions :: Aeson.Options
transactionTemplateJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldLabelModifier "transactionTemplate"
  }

instance ToSchema TransactionTemplate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions transactionTemplateJsonOptions)

instance FromJSON TransactionTemplate where
  parseJSON = Aeson.genericParseJSON transactionTemplateJsonOptions

instance ToJSON TransactionTemplate where
  toJSON = Aeson.genericToJSON transactionTemplateJsonOptions
  toEncoding = Aeson.genericToEncoding transactionTemplateJsonOptions

------------------------------------------------------------------------
-- Recurring transaction
------------------------------------------------------------------------

data RecurringTransaction = RecurringTransaction
  { recurringTransactionId :: Int64
  , recurringTransactionCompanyId :: Int64
  , recurringTransactionDayDelta :: Int64
  , recurringTransactionMonthDelta :: Int64
  , recurringTransactionStartDate :: Text
  , recurringTransactionEndDate :: Text
  , recurringTransactionTransactions :: [Int64]
  , recurringTransactionTemplate :: TransactionTemplate
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary RecurringTransaction where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

recurringTransactionJsonOptions :: Aeson.Options
recurringTransactionJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldLabelModifier "recurringTransaction"
  }

instance ToSchema RecurringTransaction where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions recurringTransactionJsonOptions)

instance FromJSON RecurringTransaction where
  parseJSON = Aeson.genericParseJSON recurringTransactionJsonOptions

instance ToJSON RecurringTransaction where
  toJSON = Aeson.genericToJSON recurringTransactionJsonOptions
  toEncoding = Aeson.genericToEncoding recurringTransactionJsonOptions

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

transactionJsonOptions :: Aeson.Options
transactionJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldLabelModifier "transaction"
  }

instance FromJSON Transaction where
  parseJSON = Aeson.genericParseJSON transactionJsonOptions

instance ToJSON Transaction where
  toJSON = Aeson.genericToJSON transactionJsonOptions
  toEncoding = Aeson.genericToEncoding transactionJsonOptions

instance ToSchema Transaction where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions transactionJsonOptions)

instance Semigroup Transaction where
  _ <> r = r

------------------------------------------------------------------------
-- Month
------------------------------------------------------------------------

data Month = Month
  { monthYear :: Int32
  , monthMonth :: Int32
  , monthTransactions :: [Transaction]
  , monthRecurring :: [(RecurringTransaction, [Text])]
  , monthBalance :: [Balance]
  , monthBankBalances :: [BankBalance]
  , monthStartBalance :: Int64
  , monthEndBalance :: Int64
  , monthLowestBalance :: Int64
  , monthNext :: Text
  , monthPrevious :: Text
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary Month where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

monthJsonOptions :: Aeson.Options
monthJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldLabelModifier "month"
  }

instance FromJSON Month where
  parseJSON = Aeson.genericParseJSON monthJsonOptions

instance ToJSON Month where
  toJSON = Aeson.genericToJSON monthJsonOptions
  toEncoding = Aeson.genericToEncoding monthJsonOptions

instance ToSchema Month where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions monthJsonOptions)

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
  { Aeson.fieldLabelModifier = fieldLabelModifier "balance"
  }

instance FromJSON Balance where
  parseJSON = Aeson.genericParseJSON balanceJsonOptions

instance ToJSON Balance where
  toJSON = Aeson.genericToJSON balanceJsonOptions
  toEncoding = Aeson.genericToEncoding balanceJsonOptions

instance ToSchema Balance where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions balanceJsonOptions)

------------------------------------------------------------------------
-- Bank Balance
------------------------------------------------------------------------

data BankBalance = BankBalance
  { bankBalanceCompanyId :: Int64
  , bankBalanceDate :: Text
  , bankBalanceMoney :: Int64
  }
  deriving (Generic, Typeable)

instance QC.Arbitrary BankBalance where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

bankBalanceJsonOptions :: Aeson.Options
bankBalanceJsonOptions = Aeson.defaultOptions
  { Aeson.fieldLabelModifier = fieldLabelModifier "bankBalance"
  }

instance FromJSON BankBalance where
  parseJSON = Aeson.genericParseJSON bankBalanceJsonOptions

instance ToJSON BankBalance where
  toJSON = Aeson.genericToJSON bankBalanceJsonOptions
  toEncoding = Aeson.genericToEncoding bankBalanceJsonOptions

instance ToSchema BankBalance where
  declareNamedSchema = Swagger.genericDeclareNamedSchema (Swagger.fromAesonOptions bankBalanceJsonOptions)
