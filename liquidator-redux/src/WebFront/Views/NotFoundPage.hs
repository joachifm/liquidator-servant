{-# LANGUAGE OverloadedStrings #-}

module WebFront.Views.NotFoundPage (render) where

import Html
import qualified WebFront.Views.SimplePage as SimplePage

render :: Html ()
render = SimplePage.render "Not found" $ do
  p_ $ text_ "The page you're looking for was not found"
