{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as LB

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson

import Options.Applicative
import Servant
import qualified Network.Wai.Handler.Warp as Warp

import Liquidator.Server

data Config = Config
  { listenPort :: Warp.Port
  , dumpConfig :: Bool
  }
  deriving (Generic, Typeable)

instance FromJSON Config
instance ToJSON Config

optsParser :: ParserInfo Config
optsParser = info (p <**> helper) idm
  where
    p :: Parser Config
    p = Config
      <$> option auto (long "port" <> metavar "PORT" <> value 3000)
      <*> switch (long "dump-config")

main :: IO ()
main = do
  cfg <- execParser optsParser
  when (dumpConfig cfg) $
    LB.putStrLn $ Aeson.encodePretty cfg
  putStrLn $ "Starting server listening on port " <> show (listenPort cfg)
  Warp.run (listenPort cfg) =<< app
