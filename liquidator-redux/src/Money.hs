{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Money
  (
    -- * Types
    Money
  , MoneyAmount

    -- * Conversion
  , moneyFromAmount
  , moneyToReal

    -- * Pretty-printing and parsing
  , ppMoney
  , parseMoney
  ) where

import Data.Word (Word32)

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | Underlying integer type used to represent money amounts.  Fixed-width
-- but should be large enuf ...
type MoneyAmount = Word32

-- | A representation of an amount of money in a currency, relative to unit
-- (e.g., 100 for two decimals).
newtype Money = MkMoney { moneyAmount :: MoneyAmount }
  -- TODO(joachifm) derive Show only for debug/devel builds (?)
  deriving (Eq, Read, Show, Generic, FromJSON, ToJSON)

-- | A 'Money' smart constructor.
moneyFromAmount :: MoneyAmount -> Money
moneyFromAmount = MkMoney

-- | A one-way conversion from 'Money' to a real value.
moneyToReal :: Money -> Float
moneyToReal = (/ 100) . (fromIntegral :: MoneyAmount -> Float) . moneyAmount

-- | Pretty-print a money amount.
--
-- >>> ppMoney (MkMoney 10)
-- "0.10"
-- >>> ppMoney (MkMoney 100)
-- "1.0"
ppMoney :: Money -> Text
ppMoney
  = (\(a, b) -> Text.pack (show a) <> "." <> Text.pack (show b))
  . (`quotRem` 100) . moneyAmount

-- | Read a textual representation of a money amount.
--
-- >>> parseMoney "0.10"
-- MkMoney 10
--
-- >>> parseMoney "0.1"
-- MkMoney 10
--
-- >>> parseMoney "1.0"
-- MkMoney 100
--
-- >>> parseMoney "1.25"
-- MkMoney 125
parseMoney
  :: Text
  -> Either String Money
parseMoney s = case Text.break (== '.') s of
  -- TODO(joachifm) a proper parser, please (?)
  (a, b) -> do
    (hd, _) <- Text.decimal a
    (tl, _) <- Text.decimal (Text.drop 1 b)
    return $! MkMoney (hd * 100 + tl)
