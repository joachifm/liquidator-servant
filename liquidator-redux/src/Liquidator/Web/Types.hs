{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}

module Liquidator.Web.Types where

import Data.Maybe (fromMaybe)

import Web.FormUrlEncoded (FromForm, ToForm)
import qualified Data.Aeson as Aeson
import qualified Web.FormUrlEncoded as Form

import Imports
import Liquidator.Types

------------------------------------------------------------------------

formOptions :: Form.FormOptions
formOptions = Form.defaultFormOptions
  { Form.fieldLabelModifier = Aeson.fieldLabelModifier aesonOptions
  }

------------------------------------------------------------------------

data TransactionFormData = TransactionFormData
  { transactionformSubject :: Text
  , transactionformAmountPri :: MoneyAmount
  , transactionformAmountSub :: Maybe MoneyAmount
  , transactionformDay :: Day
  , transactionformNotes :: [Text]
  }
  deriving (Generic)

instance FromForm TransactionFormData where
  fromForm = Form.genericFromForm formOptions

instance ToForm TransactionFormData where
  toForm = Form.genericToForm formOptions

------------------------------------------------------------------------

makeTransactionFromFormData
  :: TransactionFormData
  -> Transaction
makeTransactionFromFormData formData = Transaction
  { transactionSubject = transactionformSubject formData
  , transactionAmount = moneyFromAmount (transactionformAmountPri formData)
                                        (fromMaybe 0 (transactionformAmountSub formData))
  , transactionDay = transactionformDay formData
  , transactionNotes = concatMap splitNotes (transactionformNotes formData)
  }
