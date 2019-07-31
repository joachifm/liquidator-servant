{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StrictData #-}

module Liquidator.HttpServer (Config, run) where

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Network.Wai (Application, Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp

data Config = Config
  { port :: Warp.Port
  , addr :: Warp.HostPreference
  , keyFile :: FilePath
  , crtFile :: FilePath
  }
  deriving (Generic, FromJSON, ToJSON)

run :: Application -> IO ()
run
  = Warp.runTLS tlsSettings warpSettings
  . Gzip.gzip Gzip.def
  . ForceSSL.forceSSL
  . hsts

tlsSettings :: Warp.TlsSettings
tlsSettings
  = (Warp.tlsSettings "site.crt" "site.key")
    { Warp.onInsecure = Warp.AllowInsecure -- relies on forceSSL
    }

warpSettings :: Warp.Settings
warpSettings
  = announce
  . devSettings
  $ Warp.defaultSettings

announce :: (Warp.Settings -> Warp.Settings)
announce = \ws -> flip Warp.setBeforeMainLoop ws $ putStrLn $
      "Listening on " <> show (Warp.getHost ws)
                      <> ":"
                      <> show (Warp.getPort ws)

devSettings :: (Warp.Settings -> Warp.Settings)
devSettings
  = Warp.setPort 3000
  . Warp.setHost "127.0.0.1"
  . Warp.setLogger stdLogger
  where
    stdLogger req status _ = print req >> print status >> putStrLn ""

------------------------------------------------------------------------
-- Middleware: HTTP Strict Transport Security
------------------------------------------------------------------------

hsts :: Middleware
hsts = AddHeaders.addHeaders [
  ("Strict-Transport-Security", "max-age=31536000; includeSubDomains")
  ]
