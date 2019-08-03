{-# LANGUAGE OverloadedStrings #-}

module WebFront.Views.SimplePage (render) where

import Html

render
  :: Text
  -> Html ()
  -> Html ()
render pageTitle pageBody = do
  head_ $ do
    meta_ [ charset_ "UTF-8" ]
    title_ pageTitle_
  body_ $ do
    h1_ pageTitle_
    div_ [ id_ "main" ] $ do
      pageBody
  where
    pageTitle_ = text_ pageTitle
