{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Liquidator.Types
  (
    aesonOptions

    -- * Types
  , GenericId
  , Transaction(..)
  , Balance(..)
  , BalanceSum(..)
  , sumBalance

  , cleanNotes
  , joinNotes
  , splitNotes

    -- * Re-exports
  , module X
  ) where

import Imports

import Data.Int (Int64)
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

data Balance = Balance
  { balanceAmount :: Money
  , balanceTxCount :: Int
  , balanceStartDay :: Maybe Day
  , balanceEndDay :: Maybe Day
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

-- | Accumulate count and sum total amount in a single traversal.
data BalanceSum = BalanceSum
  { balanceSumCount  :: !Int
  , balanceSumAmount :: !Money
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

sumBalance
  :: [(GenericId, Transaction)]
  -> BalanceSum
sumBalance = foldl'
  (\(BalanceSum c acc) ((_, Transaction { transactionAmount = amnt })) ->
      BalanceSum (c + 1) (acc + amnt))
  (BalanceSum 0 0)

------------------------------------------------------------------------------

data Transaction = Transaction
  { transactionSubject :: Text
  , transactionAmount :: Money
  , transactionDay :: Day
  , transactionNotes :: [Text]
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

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
