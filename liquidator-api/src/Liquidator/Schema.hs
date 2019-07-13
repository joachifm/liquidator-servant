{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

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

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Swagger

import Servant.Swagger

------------------------------------------------------------------------
-- Role
------------------------------------------------------------------------

data Role = ReporterRole | UserRole | OwnerRole
  deriving (Eq, Generic, Typeable)

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

instance FromJSON Pagination
instance ToJSON Pagination where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToSchema Pagination

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

instance FromJSON User
instance ToJSON User where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToSchema User

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

------------------------------------------------------------------------
-- Company
------------------------------------------------------------------------

data Company = Company
  { companyId :: Int64
  , companyName :: Text
  , companyOrgNr :: Text
  }
  deriving (Generic, Typeable)

instance FromJSON Company
instance ToJSON Company where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

data TransactionType
  = Income
  | Expense
  deriving (Generic, Typeable)

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

instance FromJSON Transaction
instance ToJSON Transaction where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
instance ToSchema Transaction

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

instance FromJSON Balance
instance ToJSON Balance where toEncoding = Aeson.genericToEncoding Aeson.defaultOptions
