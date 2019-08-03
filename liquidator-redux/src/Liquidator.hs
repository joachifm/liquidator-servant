{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Liquidator where

import Imports

import Control.Monad.Except (ExceptT(ExceptT))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Casing (aesonPrefix, snakeCase)
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Time.Calendar (Day)
import Servant
import Servant.HTML.Lucid
import System.Directory (renamePath)
import Web.FormUrlEncoded (FromForm, ToForm)
import qualified Control.Exception as E
import qualified Data.Aeson as Aeson
import qualified Data.Map.Lazy as Map
import qualified Network.Wai.Handler.Warp as Warp
import qualified Web.FormUrlEncoded as Form

import IORef
import Money
import Util
import Html

------------------------------------------------------------------------------

type GenericId = Int64

------------------------------------------------------------------------------

aesonOptions :: Aeson.Options
aesonOptions = aesonPrefix snakeCase

formOptions :: Form.FormOptions
formOptions = Form.defaultFormOptions
  { Form.fieldLabelModifier = Aeson.fieldLabelModifier aesonOptions
  }

------------------------------------------------------------------------------

data Transaction = Transaction
  { transactionSubject :: Text
  , transactionAmount :: Money
  , transactionDay :: Day
  , transactionNotes :: [Text]
  }
  deriving (Eq, Generic, FromJSON, ToJSON)

------------------------------------------------------------------------------

data Handle = Handle
  { nextId :: IORef GenericId
  , transactions :: IORef (Map GenericId Transaction)
  }

newHandle :: IO Handle
newHandle = Handle
  <$> newIORef 1
  <*> newIORef mempty

withHandle
  :: (Handle -> IO a)
  -> IO a
withHandle act = E.bracket
  newHandle
  (\_ -> return ())
  act

getNextId :: Handle -> IO GenericId
getNextId = postIncIORef . nextId

------------------------------------------------------------------------------

getAllTransactions
  :: Handle
  -> IO [(GenericId, Transaction)]
getAllTransactions h
  = Map.toList <$> readIORef (transactions h)

getTransactionById
  :: Handle
  -> GenericId
  -> IO (Maybe Transaction)
getTransactionById h txid
  = Map.lookup txid <$> readIORef (transactions h)

addTransaction
  :: Handle
  -> Transaction
  -> IO GenericId
addTransaction h tx = do
  txid <- getNextId h
  atomicModifyIORef' (transactions h) $ \m ->
    (Map.insert txid tx m, txid)

updateTransaction
  :: Handle
  -> GenericId
  -> Transaction
  -> IO (Maybe Bool)
updateTransaction h txid tx = do
  mbExisting <- getTransactionById h txid
  case mbExisting of
    Just txOld ->
      atomicModifyIORef' (transactions h) $ \m ->
        let
          edited = tx /= txOld
        in
          ( if edited then Map.insert txid tx m else m
          , Just edited
          )
    Nothing ->
      return Nothing

deleteTransaction
  :: Handle
  -> GenericId
  -> IO Bool
deleteTransaction h txid = do
  mbStored <- getTransactionById h txid
  case mbStored of
    Just _ -> do
      atomicModifyIORef' (transactions h) $ \m ->
        (Map.delete txid m, True)

    Nothing ->
      return False

------------------------------------------------------------------------------

renderNav :: Html ()
renderNav = nav_ $ do
  span_ $ a_ [ href_ "/" ]       (text_ "Home")   >> text_ " | "
  span_ $ a_ [ href_ "/list" ]   (text_ "List")   >> text_ " | "
  span_ $ a_ [ href_ "/new" ]    (text_ "New")

simplePage
  :: Text
  -> Html ()
  -> Html ()
simplePage pageTitle pageBody = doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "UTF-8" ]
    title_ pageTitle_
  body_ $ do
    renderNav
    h1_ pageTitle_
    div_ [ class_ "main" ] $ do
      pageBody
  where
    pageTitle_ = text_ pageTitle

------------------------------------------------------------------------------

renderIndexPage :: Html ()
renderIndexPage = simplePage "Liquidator" $ do
  p_ $ text_ "Hello, there"

renderLoginPage :: Html ()
renderLoginPage = simplePage "Login" $ do
  form_ [ name_ "login"
        , method_ "post"
        , action_ "/login" -- TODO(joachifm) use API link
        ] $ do
    section_ $ do
      input_ [ name_ "username"
             , type_ "text"
             , placeholder_ "Username"
             , required_ "required"
             , autofocus_
             , tabindex_ "1"
             ]
      input_ [ name_ "password"
             , type_ "password"
             , placeholder_ "Passphrase"
             , required_ "required"
             , tabindex_ "2"
             ]
    input_ [ type_ "submit"
           , value_ "Login"
             , tabindex_ "3"
           ]

renderNewTransactionPage
  :: Html ()
renderNewTransactionPage = simplePage "New" $ do
  form_ [ name_ "new"
        , method_ "post"
        , action_ "/new" -- TODO(joachifm) use API link
        ] $ do
    section_ $ do
      input_ [ name_ "subject"
             , type_ "text"
             , placeholder_ "Subject"
             , required_ "required"
             , spellcheck_ "true"
             , autofocus_
             , tabindex_ "1"
             ]
      input_ [ name_ "amount_pri"
             , type_ "number"
             , placeholder_ "0"
             , min_ "0"
             , max_ "999999"
             , required_ "required"
             , tabindex_ "2"
             ]
      input_ [ name_ "amount_sub"
             , type_ "number"
             , placeholder_ "0"
             , min_ "0"
             , max_ "99"
             , value_ "0"
             , tabindex_ "3"
             ]
      input_ [ name_ "day"
             , type_ "date"
             , required_ "required"
             , value_ "2019-01-01"
             , tabindex_ "4"
             ]
    input_ [ type_ "submit"
           , value_ "Create"
           , tabindex_ "5"
           ]

renderTransactionsListPage
  :: [(GenericId, Transaction)]
  -> Html ()
renderTransactionsListPage txlist = simplePage "List" $ do
  ul_ $ do
    forM_ txlist $ \(i, tx) -> do
      li_ $ do
        a_ [ href_ ("/view/" <> showText i) ] $
          text_ (showText i <> ":" <> transactionSubject tx)

renderViewTransactionByIdPage
  :: GenericId
  -> Html ()
renderViewTransactionByIdPage txid = simplePage "View" $ do
  p_ $ do
    a_ [ href_ ("/edit/" <> showText txid) ] $ text_ "Edit"
  p_ $ do
    a_ [ href_ ("/delete/" <> showText txid) ] $ text_ "Delete"

renderEditTransactionByIdPage
  :: GenericId
  -> Maybe Transaction
  -> Html ()
renderEditTransactionByIdPage txid Nothing = simplePage "Invalid transaction id" $ do
  p_ $ do
    text_ ("Transaction id not found: " <> showText txid)
renderEditTransactionByIdPage txid (Just txdata) = simplePage "Edit" $ do
  div_ $ do
    form_ [ name_ "edit"
          , action_ ("/edit/" <> showText txid)
          , method_ "post"
          ] $ do
      section_ $ do
        input_ [ name_ "subject"
               , type_ "text"
               , value_ (transactionSubject txdata)
               , autofocus_
               , tabindex_ "1"
               ]
        input_ [ name_ "amount_pri"
               , type_ "number"
               , value_ (showText (fst (moneyToAmounts (transactionAmount txdata))))
               , min_ "0"
               , max_ "999999"
               , tabindex_ "2"
             ]
        input_ [ name_ "amount_sub"
               , type_ "number"
               , value_ (showText (snd (moneyToAmounts (transactionAmount txdata))))
               , min_ "0"
               , max_ "99"
               , tabindex_ "3"
               ]
        input_ [ name_ "day"
               , type_ "date"
               , value_ (showText (transactionDay txdata))
               , tabindex_ "4"
               ]
      input_ [ type_ "submit"
             , value_ "Apply changes"
             , tabindex_ "5"
             ]

renderDeleteTransactionByIdPage
  :: GenericId
  -> Maybe Transaction
  -> Html ()
renderDeleteTransactionByIdPage txid Nothing = simplePage "Invalid transaction id" $ do
  p_ $ do
    text_ ("Transaction id not found: " <> showText txid)
renderDeleteTransactionByIdPage txid (Just _) = simplePage "Delete" $ do
  form_ [ name_ "delete"
        , action_ ("/delete/" <> showText txid)
        , method_ "post"
        ] $ do
    input_ [ type_ "submit", value_ "Delete" ]

------------------------------------------------------------------------------

data TransactionFormData = TransactionFormData
  { transactionformSubject :: Text
  , transactionformAmountPri :: MoneyAmount
  , transactionformAmountSub :: Maybe MoneyAmount
  , transactionformDay :: Day
  }
  deriving (Generic)

instance FromForm TransactionFormData where fromForm = Form.genericFromForm formOptions
instance ToForm TransactionFormData where toForm = Form.genericToForm formOptions

makeTransactionFromFormData
  :: TransactionFormData
  -> Transaction
makeTransactionFromFormData formData = Transaction
  { transactionSubject = transactionformSubject formData
  , transactionAmount = moneyFromAmount (transactionformAmountPri formData)
                                        (fromMaybe 0 (transactionformAmountSub formData))
  , transactionDay = transactionformDay formData
  , transactionNotes = []
  }

------------------------------------------------------------------------------

type WebApi
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

------------------------------------------------------------------------------

-- index

getIndexPageHandler
  :: Handle
  -> IO (Html ())
getIndexPageHandler _ = pure $ renderIndexPage

-- /new

getNewTransactionPageHandler
  :: Handle
  -> IO (Html ())
getNewTransactionPageHandler _ = do
  pure renderNewTransactionPage

postNewTransactionHandler
  :: Handle
  -> TransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postNewTransactionHandler h formData = do
  _ <- addTransaction h (makeTransactionFromFormData formData)
  pure . addHeader "/list"
       $ NoContent

-- /list

getTransactionsListPageHandler
  :: Handle
  -> IO (Html ())
getTransactionsListPageHandler h
  = renderTransactionsListPage <$> getAllTransactions h

-- /view/:id

getViewTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getViewTransactionByIdPageHandler h txid
  = pure $ renderViewTransactionByIdPage txid

-- /edit/:id

getEditTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getEditTransactionByIdPageHandler h txid
  = renderEditTransactionByIdPage txid <$> getTransactionById h txid

postEditTransactionByIdHandler
  :: Handle
  -> GenericId
  -> TransactionFormData
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postEditTransactionByIdHandler h txid formData = do
  _ <- updateTransaction h txid (makeTransactionFromFormData formData)
  pure . addHeader "/list"
       $ NoContent

-- /delete/:id

getDeleteTransactionByIdPageHandler
  :: Handle
  -> GenericId
  -> IO (Html ())
getDeleteTransactionByIdPageHandler h txid
  = renderDeleteTransactionByIdPage txid <$> getTransactionById h txid
postDeleteTransactionByIdHandler
  :: Handle
  -> GenericId
  -> IO (Headers '[ Header "Location" Text
                  ] NoContent)
postDeleteTransactionByIdHandler h txid = do
  _ <- deleteTransaction h txid
  pure . addHeader "/list"
       $ NoContent

------------------------------------------------------------------------

webHandler
  :: Handle
  -> ServerT WebApi IO
webHandler h
  =    getIndexPageHandler h
  :<|> getTransactionsListPageHandler h
  :<|> getNewTransactionPageHandler h
  :<|> postNewTransactionHandler h
  :<|> getViewTransactionByIdPageHandler h
  :<|> getEditTransactionByIdPageHandler h
  :<|> postEditTransactionByIdHandler h
  :<|> getDeleteTransactionByIdPageHandler h
  :<|> postDeleteTransactionByIdHandler h

------------------------------------------------------------------------------

type Api
  =    WebApi
  :<|> "api" :> "v1.0" :> EmptyAPI

handler :: Handle -> ServerT Api IO
handler h
  =    webHandler h
  :<|> emptyServer

----------------------------------------------------------------------------

-- | A natural transformation from our preferred handler context to servant's
-- 'Servant.API.Handler'.
convert
  :: IO a
  -> Handler a
convert
  = Handler
  . ExceptT
  . E.try

----------------------------------------------------------------------------

api :: Proxy Api
api = Proxy

server :: Handle -> Server Api
server = hoistServer api convert . handler

app :: Handle -> Application
app = serve api . server

----------------------------------------------------------------------------

run :: IO ()
run = withHandle $ Warp.runSettings warpSettings . app

warpSettings :: Warp.Settings
warpSettings
  = announce
  . devSettings
  $ Warp.defaultSettings

announce :: (Warp.Settings -> Warp.Settings)
announce = \ws -> flip Warp.setBeforeMainLoop ws $ putStrLn $
      "Listening on " <> show (Warp.getHost ws)
                      <> ":"
                      <> show (Warp.getPort ws)

devSettings :: (Warp.Settings -> Warp.Settings)
devSettings
  = Warp.setPort 3000
  . Warp.setHost "127.0.0.1"
  . Warp.setLogger stdLogger
  where
    stdLogger req status _ = print req >> print status >> putStrLn ""

prodSettings :: (Warp.Settings -> Warp.Settings)
prodSettings
  = Warp.setPort 80
  . Warp.setHost "*"
