{-# LANGUAGE TupleSections #-}

module IORef
  ( atomicModifyIORef'_
  , postIncIORef

    -- * Re-exports
  , module X
  ) where

import Data.IORef as X
  ( IORef
  , atomicModifyIORef'
  , newIORef
  , readIORef
  )

postIncIORef :: (Integral a) => IORef a -> IO a
postIncIORef = flip atomicModifyIORef' (\i -> (i + 1, i))

atomicModifyIORef'_
  :: IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef'_ ref act = atomicModifyIORef' ref ((,()) . act)
