{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , moneyFromAmounts
  , moneyToAmounts
  , moneyToReal

    -- * Pretty-printing and parsing
  , ppMoney
  , parseMoney
  ) where

import GHC.Generics (Generic)

import Control.Applicative

import Data.String (IsString(..))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Text

import Data.Aeson (FromJSON, ToJSON)

-- | The underlying integer type used to represent money amounts.
type MoneyAmount = Integer

-- | A representation of an amount of money (in an unspecified currency).
newtype Money = MkMoney { moneyAmount :: MoneyAmount }
  deriving
    ( Eq, Ord, Generic, FromJSON, ToJSON
#ifdef TEST
    , Show
#endif
    )

instance IsString Money where
  fromString = either (error . ("fromString: " <>)) id . parseMoney . Text.pack

liftBinop
  :: (MoneyAmount -> MoneyAmount -> MoneyAmount)
  -> (Money -> Money -> Money)
liftBinop op a b = MkMoney (moneyAmount a `op` moneyAmount b)

instance Num Money where
  fromInteger x = moneyFromAmounts (fromInteger x) 0

  (+) = liftBinop (+)
  (*) = liftBinop (*)
  (-) = liftBinop (-)

  abs = id

  signum = signum . fromIntegral . moneyAmount

-- | A 'Money' smart constructor.
--
-- > moneyFromAmounts 1 25
-- MkMoney 125
--
-- > moneyFromAmounts (-1) 25
-- MkMoney -125
--
-- > moneyFromAmounts 1 (-25) -- ignore negative fractional part
-- MkMoney 125
--
-- > moneyFromAmounts 1 99
-- MkMoney 199
--
-- > moneyFromAmounts 1 100
-- MkMoney 200
moneyFromAmounts
  :: MoneyAmount -- ^ Main currency unit
  -> MoneyAmount -- ^ Fraction of main currency unit
  -> Money
moneyFromAmounts main frac = MkMoney $ main * 100 + (abs frac)

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
  . moneyToAmounts

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
parseMoney s = do
  (m, r) <- Text.decimal s
  f <- fst <$> Text.decimal (Text.drop 1 r) <|> pure 0
  return (moneyFromAmounts m f)
