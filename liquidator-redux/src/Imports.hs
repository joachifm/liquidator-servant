{-|
Module: Imports

Common imports
-}

module Imports
  ( compose
  , identity

  , -- * Re-exports
    module X
  ) where

import Data.Typeable as X (Typeable)
import GHC.Generics as X (Generic)

import Data.Functor as X
import Data.Functor.Const as X
import Data.Functor.Contravariant as X
import Data.Functor.Identity as X

import Control.Applicative as X

import Control.Monad as X
  ( ap
  , join
  , (=<<)
  , (>=>)
  , (<=<)
  )

import Data.Monoid as X
  ( Monoid(..)
  , Endo(..)
  , Sum(..)
  , Product(..)
  , Last(..)
  , First(..)
  )

import Data.Foldable as X
  ( foldl'
  , foldMap
  , foldr
  , product
  , sum
  , forM_
  , mapM_
  )

import Data.Traversable as X
  ( for
  , forM
  , mapM
  , traverse
  )

import Data.Int as X (Int64)
import Numeric.Natural as X

import Data.ByteString as X (ByteString)
import Data.Text as X (Text)

import Data.Map as X (Map)


identity :: a -> a
identity = id

compose :: [a -> a] -> (a -> a)
compose = appEndo . foldMap Endo
