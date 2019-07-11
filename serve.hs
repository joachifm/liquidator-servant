{-# LANGUAGE DeriveGeneric #-}

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

import Api

data Config = Config
  { listenPort :: Int
  , dumpConfig :: Bool
  }
  deriving (Generic, Typeable, Read, Show)

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
  when (dumpConfig cfg) $ do
    LB.putStrLn $ Aeson.encodePretty cfg
  Warp.run (listenPort cfg) app
