{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Liquidator.Types
  (
    aesonOptions

    -- * Types
  , Balance(..)
  , BalanceSum(..)
  , GenericId
  , RecurringTransaction(..)
  , Transaction(..)
  , TransactionTemplate(..)

  , sumBalance
  , cleanNotes
  , joinNotes
  , splitNotes

    -- * Re-exports
  , module X
  ) where

import Imports

import Data.Int as X (Int64)
import Data.Time.Calendar as X (Day)

import qualified Data.List as List

import Data.Text as X (Text)
import qualified Data.Text as Text

import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson

import Money as X

------------------------------------------------------------------------

type GenericId = Int64

------------------------------------------------------------------------

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

------------------------------------------------------------------------------

-- | A summary of an account balance.
data Balance = Balance
  { balanceAmount :: Money
  , balanceTxCount :: Int
  , balanceStartDay :: Maybe Day
  , balanceEndDay :: Maybe Day
  }
  deriving (Eq, Generic, Typeable, FromJSON, ToJSON)

-- | Accumulate count and sum total amount in a single traversal.
data BalanceSum = BalanceSum
  { balanceSumCount  :: !Int
  , balanceSumAmount :: !Money
  }
  deriving (Eq, Generic, Typeable, FromJSON, ToJSON)

sumBalance :: [(GenericId, Transaction)] -> BalanceSum
sumBalance = foldl' f (BalanceSum 0 0) . map snd
  where f (BalanceSum cnt amnt) x = BalanceSum (cnt + 1) (amnt + transactionAmount x)

------------------------------------------------------------------------------

data Transaction = Transaction
  { transactionSubject :: Text
  , transactionAmount :: Money
  , transactionDay :: Day
  , transactionNotes :: [Text]
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

------------------------------------------------------------------------------

data TransactionTemplate = TransactionTemplate
  { transactiontemplateSubject :: Text
  , transactiontemplateAmount :: Money
  , transactiontemplateNotes :: [Text]
  }
  deriving (Eq, Generic, Typeable, FromJSON, ToJSON)

------------------------------------------------------------------------------

data RecurringTransaction = RecurringTransaction
  { recurringtransactionTemplate :: TransactionTemplate
  , recurringtransactionDayDelta :: Maybe Int64
  , recurringtransactionMonthDelta :: Maybe Int64
  , recurringtransactionStartDate :: Day
  , recurringtransactionEndDate :: Maybe Day
  }
  deriving (Eq, Generic, Typeable, FromJSON, ToJSON)

------------------------------------------------------------------------------

-- | Combine a set of notes into a separated string.
joinNotes
  :: [Text]
  -> Text
joinNotes
  = Text.intercalate ";" . cleanNotes

-- | Break a separated string into a set of notes.
splitNotes
  :: Text
  -> [Text]
splitNotes = cleanNotes . Text.split noteSep
  where noteSep = (`elem` noteSepChars)

-- | Remove non-note strings from a set of notes.
cleanNotes
  :: [Text]
  -> [Text]
cleanNotes = List.filter (not . Text.null)
  . List.filter (`notElem` sepStr)
  . map Text.strip
  where sepStr = map (\x -> Text.pack [x]) noteSepChars

-- | Note string separators.
noteSepChars :: [Char]
noteSepChars = [',', ';', '|']
