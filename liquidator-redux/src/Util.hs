module Util
  ( showText
  , whenJust
  ) where

import Data.Text (Text)
import qualified Data.Text as Text

showText :: (Show a) => a -> Text
showText = Text.pack . show

whenJust :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
whenJust (Just x) act = act x
whenJust Nothing _    = return ()
