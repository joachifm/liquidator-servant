{-# LANGUAGE OverloadedStrings #-}

module Liquidator.Web.Views where

import Html
import Imports
import Util

import Liquidator.Types

------------------------------------------------------------------------------

mainNav :: Html ()
mainNav = nav_ $ do
  -- TODO(joachifm) use apiLink for all href below
  span_ $ a_ [ href_ "/" ]        (text_ "Home")    >> text_ " | "
  span_ $ a_ [ href_ "/list" ]    (text_ "List")    >> text_ " | "
  span_ $ a_ [ href_ "/balance" ] (text_ "Balance") >> text_ " | "
  span_ $ a_ [ href_ "/new" ]     (text_ "New")

simplePage
  :: Text
  -> Html ()
  -> Html ()
simplePage pageTitle pageBody = doctypehtml_ $ do
  head_ $ do
    meta_ [ charset_ "UTF-8" ]
    title_ (text_ ("Liquidator | " <> pageTitle))
  body_ $ do
    mainNav
    h1_ (text_ pageTitle)
    div_ [ class_ "main" ] $ do
      pageBody

------------------------------------------------------------------------------

indexPage :: Html ()
indexPage = simplePage "Index" $ do
  p_ $ text_ "Hello, there"

------------------------------------------------------------------------------

loginPage :: Html ()
loginPage = simplePage "Login" $ do
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

------------------------------------------------------------------------------

newTransactionPage
  :: Html ()
newTransactionPage = simplePage "New" $ do
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
    section_ $ do
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
    section_ $ do
      input_ [ name_ "day"
             , type_ "date"
             , required_ "required"
             , value_ "2019-01-01"
             , tabindex_ "4"
             ]
    section_ $ do
      input_ [ name_ "notes"
             , type_ "text"
             , placeholder_ "Note 1, note 2, note 3, ..."
             , tabindex_ "5"
             ]
    section_ $ do
      input_ [ type_ "submit"
             , value_ "Create"
             , tabindex_ "6"
             ]

------------------------------------------------------------------------------

transactionsListPage
  :: [(GenericId, Transaction)]
  -> Html ()
transactionsListPage txlist = simplePage "List" $ do
  ul_ $ do
    forM_ txlist $ \(i, tx) -> do
      li_ $ do
        -- TODO(joachifm) use apiLink here
        a_ [ href_ ("/view/" <> showText i) ] $
          text_ (showText i <> ":" <> transactionSubject tx)

------------------------------------------------------------------------

viewTransactionByIdPage
  :: GenericId
  -> Html ()
viewTransactionByIdPage txid = simplePage "View" $ do
  p_ $ do
    -- TODO(joachifm) use apiLink here
    a_ [ href_ ("/edit/" <> showText txid) ] $ text_ "Edit"
  p_ $ do
    -- TODO(joachifm) use apiLink here
    a_ [ href_ ("/delete/" <> showText txid) ] $ text_ "Delete"

------------------------------------------------------------------------

editTransactionByIdPage
  :: GenericId
  -> Maybe Transaction
  -> Html ()
editTransactionByIdPage txid Nothing = simplePage "Invalid transaction id" $ do
  p_ $ text_ ("Transaction id not found: " <> showText txid)
editTransactionByIdPage txid (Just txdata) = simplePage "Edit" $ do
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
    section_ $ do
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
    section_ $ do
      input_ [ name_ "day"
             , type_ "date"
             , value_ (showText (transactionDay txdata))
             , tabindex_ "4"
             ]
    section_ $ do
      input_ [ name_ "notes"
             , type_ "text"
             , value_ (joinNotes (transactionNotes txdata))
             , tabindex_ "5"
             ]
    section_ $ do
      input_ [ type_ "submit"
             , value_ "Apply changes"
             , tabindex_ "6"
             ]

------------------------------------------------------------------------

deleteTransactionByIdPage
  :: GenericId
  -> Maybe Transaction
  -> Html ()
deleteTransactionByIdPage txid Nothing = simplePage "Invalid transaction id" $ do
  p_ $ text_ ("Transaction id not found: " <> showText txid)
deleteTransactionByIdPage txid (Just _) = simplePage "Delete" $ do
  form_ [ name_ "delete"
        , action_ ("/delete/" <> showText txid)
        , method_ "post"
        ] $ do
    input_ [ type_ "submit", value_ "Delete" ]

------------------------------------------------------------------------

viewBalancePage
  :: Balance
  -> Html ()
viewBalancePage bal = simplePage "Balance" $ do
  p_ $ text_ $
    "Balance as of "
    <> maybe mempty ((<> " --- ") . showText) (balanceStartDay bal)
    <> maybe "today" showText                 (balanceEndDay bal)
  p_ $ text_ $
    "total: " <> ppMoney (balanceAmount bal)
  p_ $ text_ $
    "txcount: " <> showText (balanceTxCount bal)
