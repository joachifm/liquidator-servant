{-# LANGUAGE Safe #-}

module IdSupply
  (
    -- * Types
    IdSupply

    -- * Methods
  , newIdSupply
  , supplyNextId

    -- * Re-exports
  , Int64
  ) where

import Data.Int (Int64)
import Data.IORef (IORef, newIORef, atomicModifyIORef')

type IdSupply = IORef Int64

newIdSupply :: IO IdSupply
newIdSupply = newIORef 1

supplyNextId :: IdSupply -> IO Int64
supplyNextId = flip atomicModifyIORef' (\i -> (i + 1, i))
