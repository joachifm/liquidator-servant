{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator.Web.Api
  ( Api
  , api
    -- * Re-exports
  , module X
  ) where

import Lucid as X (Html)
import Servant
import Servant.HTML.Lucid

import Imports

import Liquidator.Types as X
import Liquidator.Web.Types as X

type Api
  =    Get '[HTML]
           (Html ())

  -- /list
  :<|> "list" :>
       Get '[HTML]
           (Html ())

  -- /new
  :<|> "new" :>
       Get '[HTML]
           (Html ())

  :<|> "new" :>
       ReqBody '[FormUrlEncoded] TransactionFormData :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text ]
                     NoContent)

  -- /view
  :<|> "view" :>
       Capture "id" GenericId :>
       Get '[HTML]
           (Html ())

  -- /edit
  :<|> "edit" :>
       Capture "id" GenericId :>
       Get '[HTML]
           (Html ())

  :<|> "edit" :>
       Capture "id" GenericId :>
       ReqBody '[FormUrlEncoded] TransactionFormData :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text ]
                     NoContent)

  -- /delete
  :<|> "delete" :>
       Capture "id" GenericId :>
       Get '[HTML]
           (Html ())

  :<|> "delete" :>
       Capture "id" GenericId :>
       Verb 'POST 301 '[PlainText]
            (Headers '[ Header "Location" Text ]
                     NoContent)
  -- /balance
  :<|> "balance" :>
       QueryParam "start_date" Day :>
       QueryParam "end_date" Day :>
       Get '[HTML]
           (Html ())

api :: Proxy Api
api = Proxy
