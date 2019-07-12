module Server
  ( Handle
  , newHandle
  , app
  ) where

import Data.Maybe
import Control.Monad
import Control.Exception (throwIO, try)
import Control.Monad.Except (ExceptT(..))
import Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)

import Servant

import Api

------------------------------------------------------------------------
-- App environment
------------------------------------------------------------------------

-- TODO(joachifm) IORef not thread safe, use MVar(?)

-- | An opaque resource handle.
data Handle = Handle
  { transactionDb :: !(IORef (Map Int64 Transaction))
  , nextId :: !(IORef Int64)
  }

dummyDb :: Map Int64 Transaction
dummyDb = Map.fromList . zipWith assign [ 1 .. ] $
  [ Transaction 0 1 Nothing "1970-01-01" 1000 Income "" ""
  , Transaction 0 1 Nothing "1970-01-01" 1000 Income "" ""
  ]
  where
    assign txid tx = (txid, tx { transactionId = txid })

newHandle :: IO Handle
newHandle = Handle <$> IORef.newIORef dummyDb <*> IORef.newIORef 1

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

getTransactionById :: Handle -> Int64 -> Int64 -> IO Transaction
getTransactionById ctx txid companyId_ = do
  db <- IORef.readIORef (transactionDb ctx)
  case Map.lookup txid (Map.filter ((== companyId_) . transactionCompanyId) db) of
    Nothing  -> throwIO err404
    Just elt -> return elt

addTransaction :: Handle -> Transaction -> IO Transaction
addTransaction ctx tx = do
  txid <- IORef.atomicModifyIORef' (nextId ctx) (\s -> (s + 1, s))
  let tx' = tx { transactionId = txid }
  -- TODO(joachifm) silently overwrites on duplicate pkey
  IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    (Map.insert txid tx s, ())
  return tx'

updateTransaction :: Handle -> Transaction -> IO Transaction
updateTransaction ctx tx = do
  mbTx <- IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    let
      txid = transactionId tx
    in
      case Map.lookup txid s of
        Just tx0 ->
          let tx1 = tx0 <> tx -- right-biased merge
          in (Map.insert txid tx1 s, Just tx1)
        _ -> (s, Nothing)
  case mbTx of
    Just tx' -> return tx'
    Nothing  -> throwIO err404

deleteTransaction :: Handle -> Int64 -> Int64 -> IO NoContent
deleteTransaction ctx txid companyId_ = do
  deleted <- IORef.atomicModifyIORef' (transactionDb ctx) $ \s ->
    case Map.lookup txid (Map.filter ((== companyId_) . transactionCompanyId) s) of
      Just _  -> (Map.delete txid s, True)
      Nothing -> (s, False)
  if deleted
    then return NoContent
    else throwIO err404

------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------

-- | The underlying IO implementation of the API handler, to be hoisted into
-- the servant handler context.
server' :: Handle -> ServerT Api IO
server' ctx = return swaggerDoc
  :<|> getTransactionById ctx
  :<|> addTransaction ctx
  :<|> updateTransaction ctx
  :<|> deleteTransaction ctx

-- | A natural transformation from our preferred handler context
-- to the one expected by servant.
nt :: IO a -> Handler a
nt = Handler . ExceptT . (try :: IO a -> IO (Either ServantErr a))

server :: Handle -> Server Api
server ctx = hoistServer api nt (server' ctx)

app :: Handle -> Application
app ctx = serve api (server ctx)
