{-# LANGUAGE OverloadedStrings #-}

module WebFront.Views.Template (render) where

import Html
import qualified WebFront.Views.SimplePage as SimplePage

render :: Html ()
render = SimplePage.render "Template" $ do
  p_ $ text_ "Placeholder"
