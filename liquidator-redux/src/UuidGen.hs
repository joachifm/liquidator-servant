{-|
Module: UuidGen
-}

module UuidGen
  ( -- * Exceptions
    UiidGenException

    -- * Methods
  , nextUuid

    -- * Re-exports
  , Text
  )
  where

import           Data.Text (Text)

import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E

import qualified Data.UUID    as UUID (toText)
import qualified Data.UUID.V1 as UUID

------------------------------------------------------------------------

-- | Exhausted UUID supply.
data UiidGenException = MkUuidGenException
  deriving (Show)

instance E.Exception UiidGenException

------------------------------------------------------------------------

-- | A variant of 'Data.UUID.V1.nextUUID' that retries generation in case of
-- failure (i.e., when new ids are requested too quickly), upto some maximum
-- number of retries.
--
-- Throws 'UuidGen.UiidGenException' after reaching the maximum number of
-- retries.
nextUuid :: IO Text
nextUuid = loop (1000::Int)
  where
    loop 0 = E.throwIO MkUuidGenException
    loop n = maybe (threadDelay 250 >> loop (n - 1)) (pure . UUID.toText) =<< UUID.nextUUID
