{-# LANGUAGE OverloadedStrings #-}

module WebFront.Views.CounterPage (render) where

import Html
import Util
import qualified WebFront.Views.SimplePage as SimplePage

render :: Integer -> Html ()
render z = SimplePage.render "Counter" $ do
  p_ $ text_ ("Counter: " <> showText z)
