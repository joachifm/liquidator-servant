module Server (app) where

import Control.Exception (throwIO, try)
import Control.Monad.Except (ExceptT(..))

import Servant

import Api

------------------------------------------------------------------------
-- Transaction
------------------------------------------------------------------------

getTransactionById :: Int64 -> Int64 -> IO Transaction
getTransactionById _ _ = throwIO err404

addTransaction :: Transaction -> IO Transaction
addTransaction _ = throwIO err404

updateTransaction :: Transaction -> IO Transaction
updateTransaction _ = throwIO err404

deleteTransaction :: Int64 -> Int64 -> IO NoContent
deleteTransaction _ _ = throwIO err404

------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------

-- | The underlying IO implementation of the API handler, to be hoisted into
-- the servant handler context.
server' :: ServerT Api IO
server' = return swaggerDoc
  :<|> getTransactionById
  :<|> addTransaction
  :<|> updateTransaction
  :<|> deleteTransaction

-- | A natural transformation from our preferred handler context
-- to the one expected by servant.
nt :: IO a -> Handler a
nt = Handler . ExceptT . (try :: IO a -> IO (Either ServantErr a))

server :: Server Api
server = hoistServer api nt server'

app :: Application
app = serve api server
