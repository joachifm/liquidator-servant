{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Wai.Middleware.Hsts

HTTP Strict Transport Security
-}

module Wai.Middleware.Hsts
  ( Config(..)
  , defaultConfig
  , hsts
  , hstsWith
  )
  where

import Data.ByteString (ByteString)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import qualified Network.Wai.Middleware.AddHeaders as AddHeaders
import Network.Wai (Middleware)

-- | Middleware configuration, pass to 'hstsWith'.
data Config = Config
  { maxAge :: Int
  , includeSubDomains :: Bool
  }

-- | Default 'Config'.
defaultConfig :: Config
defaultConfig = Config
  { maxAge = 31536000
  , includeSubDomains = True
  }

hsts :: Middleware
hsts = hstsWith defaultConfig

hstsWith :: Config -> Middleware
hstsWith cfg = AddHeaders.addHeaders [
  ("Strict-Transport-Security", hstsHeaderValue cfg)
  ]

------------------------------------------------------------------------
-- Internals
------------------------------------------------------------------------

hstsHeaderValue :: Config -> ByteString
hstsHeaderValue cfg = Text.encodeUtf8 $
  "max-age=" <> Text.pack (show $ maxAge cfg) <>
  (if includeSubDomains cfg then "; includeSubDomains" else "")
