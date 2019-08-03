module Html
  ( text_

    -- * Re-exports
  , module X
  ) where

import Data.Text as X (Text)
import Lucid as X (Html, toHtml)
import Lucid.Html5 as X

text_ :: Text -> Html ()
text_ = toHtml
