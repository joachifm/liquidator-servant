{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Wai.Middleware.VerifyCsrToken
  ( Config(..)
  , defaultConfig
  , verifyCsrToken
  ) where

import Data.ByteString (ByteString)

import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai
import Web.Cookie (parseCookies)

data Config = Config
  { cookieName :: ByteString
  , headerName :: HeaderName
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { cookieName = "XSRF-TOKEN"
  , headerName = "X-XSRF-TOKEN"
  }

verifyCsrToken :: Middleware
verifyCsrToken = verifyCsrTokenWith defaultConfig

-- TODO(joachifm) compare header with cookied value?
-- TODO(joachifm) check if X-XSRF-TOKEN header already set?
verifyCsrTokenWith :: Config -> Middleware
verifyCsrTokenWith cfg = ifRequest ((== methodPost) . requestMethod) $ \app req resp -> do
  case getCookiedXsrfToken (cookieName cfg) req of
    Nothing ->
      app req resp
    Just xsrfToken -> do
      app (req { requestHeaders = (headerName cfg, xsrfToken) : requestHeaders req })
          resp

getCookiedXsrfToken
  :: ByteString
  -> Request
  -> Maybe ByteString
getCookiedXsrfToken cookieName_ req
  = lookup cookieName_ =<<
    parseCookies <$> lookup "Cookie" (requestHeaders req)
