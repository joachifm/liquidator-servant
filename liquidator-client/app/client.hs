module Main (main) where

import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as LB

import Data.Aeson (ToJSON)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Servant.Client
import qualified Data.Aeson.Encode.Pretty as Aeson

import Liquidator.Client

runClientM_ :: (ToJSON e) => ClientEnv -> ClientM e -> IO ()
runClientM_ mgr act = do
  res <- runClientM act mgr
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right v  -> LB.putStrLn $ Aeson.encodePretty v

main :: IO ()
main = do
  mgr <- mkClientEnv <$> newManager defaultManagerSettings
                     <*> pure (BaseUrl Http "localhost" 8081 "")
  runClientM_ mgr $ getTransactionById 1 1
