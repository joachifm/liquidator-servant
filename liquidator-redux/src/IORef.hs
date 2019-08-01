{-# LANGUAGE Safe #-}
{-# LANGUAGE TupleSections #-}

module IORef
  ( atomicModifyIORef'_
  , postIncIORef
  ) where

import Data.Int
import Data.IORef

postIncIORef :: (Integral a) => IORef a -> IO a
postIncIORef = flip atomicModifyIORef' (\i -> (i + 1, i))
{-# INLINE postIncIORef #-}
{-# SPECIALISE postIncIORef :: IORef Int64 -> IO Int64 #-}

atomicModifyIORef'_
  :: IORef a
  -> (a -> a)
  -> IO ()
atomicModifyIORef'_ ref act = atomicModifyIORef' ref ((,()) . act)
{-# INLINE atomicModifyIORef'_ #-}
