{-# LANGUAGE Rank2Types #-}

module Lens
  ( (?~)
    -- * Re-exports
  , module X
  ) where

import Lens.Family2       as X
import Lens.Family2.Stock as X
import Lens.Family2.TH    as X

-- Not in lens-family
infixr 4 ?~
(?~) :: Setter a a' b (Maybe b') -> b' -> a -> a'
l ?~ v = l .~ Just v
