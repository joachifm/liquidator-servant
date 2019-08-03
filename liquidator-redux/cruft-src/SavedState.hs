
data SavedState = SavedState
  { savedstateNextId :: GenericId
  , savedstateTransactions :: Map GenericId Transaction
  }
  deriving (Generic, FromJSON, ToJSON)

saveHandleState
  :: Handle
  -> IO SavedState
saveHandleState h = SavedState
  <$> readIORef (h^.nextId)
  <*> readIORef (h^.transactions)

restoreHandleState
  :: Handle
  -> SavedState
  -> IO ()
restoreHandleState h s = do
  writeIORef (h^.nextId)       (savedstateNextId s)
  writeIORef (h^.transactions) (savedstateTransactions s)

dumpState
  :: Handle
  -> FilePath
  -> IO ()
dumpState h outFile = do
  let tmpFile = outFile <> ".tmp"
  Aeson.encodeFile tmpFile =<< saveHandleState h
  renamePath tmpFile outFile

loadState
  :: Handle
  -> FilePath
  -> IO ()
loadState h inFile = do
  s <- Aeson.decodeFileStrict' inFile
       `E.catch` \(_::E.IOException) -> return Nothing
  case s of
    Just v  -> restoreHandleState h v
    Nothing -> return ()

withHandle
  :: (Handle -> IO a)
  -> IO a
withHandle act = E.bracket
  (newHandle >>= \h -> loadState h stateFile >> return h)
  (flip dumpState stateFile)
  act
  where
    stateFile = "_liquidator.state"
