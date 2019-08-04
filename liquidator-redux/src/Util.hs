{-|
Module: Util

A grab-bag of utilities.
-}

module Util
  ( showText
  , whenJust
  ) where

import Data.String (IsString(..))

showText :: (IsString s, Show a) => a -> s
showText = fromString . show

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) act = act x
whenJust Nothing _    = return ()
