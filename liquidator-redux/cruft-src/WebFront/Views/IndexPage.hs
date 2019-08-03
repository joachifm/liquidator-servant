{-# LANGUAGE OverloadedStrings #-}

module WebFront.Views.IndexPage (render) where

import Html
import qualified WebFront.Views.SimplePage as SimplePage

render :: Html ()
render = SimplePage.render "Index" $ do
  p_ $ text_ "Welcome"
