{-# LANGUAGE TupleSections #-}

module IORef
  ( atomicModifyIORef'_
  , postIncIORef
  ) where

import Data.IORef

postIncIORef :: (Integral a) => IORef a -> IO a
postIncIORef = flip atomicModifyIORef' (\i -> (i + 1, i))

atomicModifyIORef'_
  :: IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef'_ ref act = atomicModifyIORef' ref ((,()) . act)
