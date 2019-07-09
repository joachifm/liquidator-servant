{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Schema
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
instance ToJSON Role
instance ToSchema Role

------------------------------------------------------------------------
-- Pagination
------------------------------------------------------------------------

type Url = Text

data Pagination = Pagination
  { paginationPage :: Int64
  , next :: Url
  , previous :: Url
  }
  deriving (Generic, Typeable)

instance FromJSON Pagination
instance ToJSON Pagination
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
instance ToJSON User
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
instance ToJSON Company

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
instance ToJSON TransactionType

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
instance ToJSON Transaction
instance ToSchema Transaction

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
instance ToJSON Balance
