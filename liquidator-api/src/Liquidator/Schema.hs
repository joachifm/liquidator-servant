{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Liquidator.Schema
  ( -- * Re-exports
    Int64
  , Int32
  , Text

    -- * Schema types
  , Balance(..)
  , BankBalance(..)
  , Company(..)
  , Month(..)
  , Pagination(..)
  , RecurringTransaction(..)
  , Role(..)
  , Transaction(..)
  , TransactionTemplate(..)
  , TransactionType(..)
  , Url
  , User(..)
  , UserCreate(..)

  -- Ad-hoc, not in upstream spec
  , LoginData(..)
  , LoginSuccess(..)
  , Refresh(..)
  , RefreshResult(..)
  ) where

import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Text as Text

import qualified Test.QuickCheck as QC
import qualified Test.QuickCheck.Arbitrary as QC
import qualified Test.QuickCheck.Arbitrary.Generic as QC

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.TH as Aeson
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger (ToSchema(..), ToParamSchema(..))
import qualified Data.Swagger as Swagger
import Web.HttpApiData
import Servant.Swagger

import Liquidator.SchemaTH

------------------------------------------------------------------------
-- Orphan Arbitrary instances
------------------------------------------------------------------------

instance QC.Arbitrary Text where
  arbitrary = QC.oneof [ pure mempty, pure (Text.pack "<placeholder>") ]

------------------------------------------------------------------------
-- Role
------------------------------------------------------------------------

data Role = Role_Reporter | Role_User | Role_Owner
  deriving (Eq, Generic, Typeable, Show)

instance QC.Arbitrary Role where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''Role)

instance ToSchema Role where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

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
-- Pagination
------------------------------------------------------------------------

type Url = Text

data Pagination = Pagination
  { pagination_page :: Int64
  , pagination_next :: Url
  , pagination_previous :: Url
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary Pagination where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''Pagination)

instance ToSchema Pagination where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- User
------------------------------------------------------------------------

data User = User
  { user_id :: Int64
  , user_first_name :: Text
  , user_last_name :: Text
  , user_email :: Text
  , user_companies :: [Int64]
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary User where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''User)

instance ToSchema User where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

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

------------------------------------------------------------------------
-- UserCreate
------------------------------------------------------------------------

data UserCreate = UserCreate
  { userCreate_first_name :: Text
  , userCreate_last_name :: Text
  , userCreate_email :: Text
  , userCreate_password :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary UserCreate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''UserCreate)

instance ToSchema UserCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

data Company = Company
  { company_id :: Int64
  , company_name :: Text
  , company_org_nr :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary Company where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''Company)

instance ToSchema Company where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Transaction type
------------------------------------------------------------------------

data TransactionType
  = TransactionType_Income
  | TransactionType_Expense
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary TransactionType where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''TransactionType)

instance ToSchema TransactionType where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

-- TODO(joachifm) generics?
instance ToParamSchema TransactionType

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

data Transaction = Transaction
  { transaction_id :: Int64
  , transaction_company_id :: Int64
  , transaction_recurring_id :: Maybe Int64
  , transaction_date :: Text
  , transaction_money :: Int64
  , transaction_type :: TransactionType
  , transaction_description :: Text
  , transaction_notes :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary Transaction where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''Transaction)

instance ToSchema Transaction where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

instance Semigroup Transaction where
  _ <> r = r

------------------------------------------------------------------------
-- Transaction template
------------------------------------------------------------------------

data TransactionTemplate = TransactionTemplate
  { transactionTemplate_id :: Int64
  , transactionTemplate_money :: Int64
  , transactionTemplate_type :: TransactionType
  , transactionTemplate_description :: Text
  , transactionTemplate_note :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary TransactionTemplate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''TransactionTemplate)

instance ToSchema TransactionTemplate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Recurring transaction
------------------------------------------------------------------------

data RecurringTransaction = RecurringTransaction
  { recurringTransaction_id :: Int64
  , recurringTransaction_company_id :: Int64
  , recurringTransaction_day_delta :: Int64
  , recurringTransaction_month_delta :: Int64
  , recurringTransaction_start_date :: Text
  , recurringTransaction_end_date :: Text
  , recurringTransaction_transactions :: [Int64]
  , recurringTransaction_template :: TransactionTemplate
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary RecurringTransaction where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''RecurringTransaction)

instance ToSchema RecurringTransaction where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Balance
------------------------------------------------------------------------

data Balance = Balance
  { balance_company_id :: Int64
  , balance_date :: Text
  , balance_money :: Int64
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary Balance where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''Balance)

instance ToSchema Balance where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Bank Balance
------------------------------------------------------------------------

data BankBalance = BankBalance
  { bankBalance_company_id :: Int64
  , bankBalance_date :: Text
  , bankBalance_money :: Int64
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary BankBalance where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''BankBalance)

instance ToSchema BankBalance where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Month
------------------------------------------------------------------------

data Month = Month
  { month_year :: Int32
  , month_month :: Int32
  , month_transactions :: [Transaction]
  , month_recurring :: [(RecurringTransaction, [Text])]
  , month_balance :: [Balance]
  , month_bank_balances :: [BankBalance]
  , month_start_balance :: Int64
  , month_end_balance :: Int64
  , month_lowest_balance :: Int64
  , month_next :: Text
  , month_previous :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary Month where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''Month)

instance ToSchema Month where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions
