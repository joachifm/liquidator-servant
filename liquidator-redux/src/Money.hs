{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

{-|
Module: Money

A representation of money amounts.
-}

module Money
  (
    -- * Types
    MoneyAmount

#ifndef TEST
  , Money
#else
  , Money(..)
#endif

    -- * Conversion
  , moneyFromAmount
  , moneyToAmounts
  , moneyToReal

    -- * Pretty-printing and parsing
  , ppMoney
  , parseMoney
  ) where

import GHC.Generics (Generic)

import Data.Word (Word32)

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Data.Aeson (FromJSON, ToJSON)

-- | Underlying integer type used to represent money amounts.  Fixed-width
-- but should be large enuf ...
type MoneyAmount = Word32

-- | A representation of an amount of money (in an unspecified currency).
--
-- Internally, the amount is specified relative to unit 100, that allows for
-- upto two decimals.
newtype Money = MkMoney { moneyAmount :: MoneyAmount }
  deriving
    ( Eq, Ord, Generic, FromJSON, ToJSON
#ifdef TEST
    , Show, Read
#endif
    )

instance IsString Money where
  fromString x = case parseMoney (Text.pack x) of
    Right v -> v
    Left e  -> error ("fromString: " <> e)

liftBinop op a b = MkMoney (moneyAmount a `op` moneyAmount b)

instance Num Money where
  fromInteger x = moneyFromAmount (fromInteger x) 0

  (+) = liftBinop (+)
  (-) = liftBinop (-)
  (*) = liftBinop (*)

  abs = id

-- | A 'Money' smart constructor.
--
-- > moneyFromAmount 1 25
-- MkMoney 125
moneyFromAmount
  :: MoneyAmount
  -> MoneyAmount
  -> Money
moneyFromAmount a b = MkMoney (a * 100 + b)

moneyToAmounts
  :: Money
  -> (MoneyAmount, MoneyAmount)
moneyToAmounts = (`quotRem` 100) . moneyAmount

-- | A one-way conversion from 'Money' to a real value.
moneyToReal :: Money -> Float
moneyToReal = (/ 100) . (fromIntegral :: MoneyAmount -> Float) . moneyAmount

-- | Pretty-print a money amount.
--
-- >>> ppMoney (MkMoney 10)
-- "0.10"
-- >>> ppMoney (MkMoney 100)
-- "1.0"
ppMoney
  :: Money
  -> Text
ppMoney
  = (\(a, b) -> Text.pack (show a <> "." <> show b))
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
    return $! moneyFromAmount hd tl
