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
  , joinNotes
  , splitNotes

    -- * Re-exports
  , module X
  ) where

import GHC.Generics (Generic)

import Data.Int (Int64)
import Data.Time.Calendar as X (Day)

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

data Transaction = Transaction
  { transactionSubject :: Text
  , transactionAmount :: Money
  , transactionDay :: Day
  , transactionNotes :: [Text]
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

joinNotes
  :: [Text]
  -> Text
joinNotes
  = Text.intercalate ";"

splitNotes
  :: Text
  -> [Text]
splitNotes
  = Text.split (`elem` [',', ';', '|'])
