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
  , UserCompanyRelation(..)
  , UserCreate(..)
  , UserUpdate(..)
  , UserKeys(..)

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
-- UserCompanyRelation
------------------------------------------------------------------------

data UserCompanyRelation = UserCompanyRelation
  { userCompanyRelation_company_id :: Int64
  , userCompanyRelation_user_id :: Int64
  , userCompanyRelation_role :: Role
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary UserCompanyRelation where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''UserCompanyRelation)

instance ToSchema UserCompanyRelation where
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
-- UserUpdate
------------------------------------------------------------------------

data UserUpdate = UserUpdate
  { userUpdate_first_name :: Text
  , userUpdate_last_name :: Text
  , userUpdate_email :: Text
  , userUpdate_password :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary UserUpdate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''UserUpdate)

instance ToSchema UserUpdate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- UserKeys
------------------------------------------------------------------------

data UserKeys = UserKeys
  { userKeys_refresh :: Text
  , userKeys_access :: Text
  , userKeys_user :: User
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary UserKeys where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''UserKeys)

instance ToSchema UserKeys where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

data Company = Company
  { company_id :: Int64
  , company_name :: Text
  , company_org_nr :: Text
  , company_users :: [UserCompanyRelation]
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary Company where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''Company)

instance ToSchema Company where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- CompanyCreate
------------------------------------------------------------------------

data CompanyCreate = CompanyCreate
  { companyCreate_name :: Text
  , companyCreate_org_nr :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary CompanyCreate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''CompanyCreate)

instance ToSchema CompanyCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- CompanyUpdate
------------------------------------------------------------------------

data CompanyUpdate = CompanyUpdate
  { companyUpdate_id :: Int64
  , companyUpdate_name :: Text
  , companyUpdate_org_nr :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary CompanyUpdate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''CompanyUpdate)

instance ToSchema CompanyUpdate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Transaction type
------------------------------------------------------------------------

data TransactionType
  = TransactionType_IN
  | TransactionType_EX
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
  , transaction_money :: Int64
  , transaction_type :: TransactionType
  , transaction_description :: Text
  , transaction_notes :: Text
  , transaction_company_id :: Int64
  , transaction_recurring_id :: Maybe Int64
  , transaction_date :: Text
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
-- Transaction template
------------------------------------------------------------------------

data TransactionTemplateCreate = TransactionTemplateCreate
  { transactionTemplateCreate_money :: Int64
  , transactionTemplateCreate_type :: TransactionType
  , transactionTemplateCreate_description :: Text
  , transactionTemplateCreate_note :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary TransactionTemplateCreate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''TransactionTemplateCreate)

instance ToSchema TransactionTemplateCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Transaction base
------------------------------------------------------------------------

data TransactionBase = TransactionBase
  { transactionBase_money :: Int64
  , transactionBase_type :: TransactionType
  , transactionBase_description :: Text
  , transactionBase_note :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary TransactionBase where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''TransactionBase)

instance ToSchema TransactionBase where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Transaction create
------------------------------------------------------------------------

data TransactionCreate = TransactionCreate
  { transactionCreate_money :: Int64
  , transactionCreate_type :: TransactionType
  , transactionCreate_description :: Text
  , transactionCreate_notes :: Text
  , transactionCreate_company_id :: Int64
  , transactionCreate_recurring_id :: Maybe Int64
  , transactionCreate_date :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary TransactionCreate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''TransactionCreate)

instance ToSchema TransactionCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Recurring transaction
------------------------------------------------------------------------

data RecurringTransaction = RecurringTransaction
  { recurringTransaction_id :: Int64
  , recurringTransaction_template :: TransactionTemplate
  , recurringTransaction_company_id :: Int64
  , recurringTransaction_day_delta :: Int64
  , recurringTransaction_month_delta :: Int64
  , recurringTransaction_start_date :: Text
  , recurringTransaction_end_date :: Text
  , recurringTransaction_transactions :: [Int64]
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary RecurringTransaction where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''RecurringTransaction)

instance ToSchema RecurringTransaction where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Recurring transaction
------------------------------------------------------------------------

data RecurringTransactionBase = RecurringTransactionBase
  { recurringTransactionBase_company_id :: Int64
  , recurringTransactionBase_day_delta :: Int64
  , recurringTransactionBase_month_delta :: Int64
  , recurringTransactionBase_start_date :: Text
  , recurringTransactionBase_end_date :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary RecurringTransactionBase where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''RecurringTransactionBase)

instance ToSchema RecurringTransactionBase where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Recurring transaction
------------------------------------------------------------------------

data RecurringTransactionCreate = RecurringTransactionCreate
  { recurringTransactionCreate_template :: TransactionTemplateCreate
  , recurringTransactionCreate_company_id :: Int64
  , recurringTransactionCreate_day_delta :: Int64
  , recurringTransactionCreate_month_delta :: Int64
  , recurringTransactionCreate_start_date :: Text
  , recurringTransactionCreate_end_date :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary RecurringTransactionCreate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''RecurringTransactionCreate)

instance ToSchema RecurringTransactionCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Recurring transaction
------------------------------------------------------------------------

data RecurringTransactionUpdate = RecurringTransactionUpdate
  { recurringTransactionUpdate_id :: Int64
  , recurringTransactionUpdate_template :: TransactionTemplateCreate
  , recurringTransactionUpdate_company_id :: Int64
  , recurringTransactionUpdate_day_delta :: Int64
  , recurringTransactionUpdate_month_delta :: Int64
  , recurringTransactionUpdate_start_date :: Text
  , recurringTransactionUpdate_end_date :: Text
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary RecurringTransactionUpdate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''RecurringTransactionUpdate)

instance ToSchema RecurringTransactionUpdate where
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
-- Bank Balance
------------------------------------------------------------------------

data BankBalanceCreate = BankBalanceCreate
  { bankBalanceCreate_company_id :: Int64
  , bankBalanceCreate_date :: Text
  , bankBalanceCreate_money :: Int64
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary BankBalanceCreate where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''BankBalanceCreate)

instance ToSchema BankBalanceCreate where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

------------------------------------------------------------------------
-- Month
------------------------------------------------------------------------

data MonthRecurring = MonthRecurring
  { monthRecurring_recurring :: RecurringTransaction
  , monthRecurring_dates :: [Text]
  }
  deriving (Generic, Typeable, Eq, Show)

instance QC.Arbitrary MonthRecurring where
  arbitrary = QC.genericArbitrary
  shrink = QC.genericShrink

$(deriveJSON ''MonthRecurring)

instance ToSchema MonthRecurring where
  declareNamedSchema = Swagger.genericDeclareNamedSchema schemaOptions

data Month = Month
  { month_year :: Int32
  , month_month :: Int32
  , month_transactions :: [Transaction]
  , month_recurring :: MonthRecurring
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
