{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Wai.Middleware.VerifyCsrToken
  ( Config(..)
  , defaultConfig
  , verifyCsrToken
  , xsrfToken
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

import Network.HTTP.Types (parseQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai
import Web.Cookie (parseCookies)

data Config = Config
  { cookieName :: ByteString -- ^ Name of cookie carrying the XSRF token
  , headerName :: HeaderName -- ^ Name of header holding XSRF token
  , fieldName :: ByteString  -- ^ Name of input field used to hold XSRF token in forms
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { cookieName = "XSRF-TOKEN"
  , headerName = "X-XSRF-TOKEN"
  , fieldName = "xsrf_token"
  }

xsrfToken :: Config -> Middleware
xsrfToken cfg = verifyCsrTokenWith cfg . getPostFormXsrfToken cfg

verifyCsrToken :: Middleware
verifyCsrToken = verifyCsrTokenWith defaultConfig

-- TODO(joachifm) compare header with cookied value?
-- TODO(joachifm) check if X-XSRF-TOKEN header already set?
-- TODO(joachifm) read XSRF-TOKEN from hidden POST parameters
--   look for method POST and Content-Type: application/x-www-form-urlencoded
verifyCsrTokenWith :: Config -> Middleware
verifyCsrTokenWith cfg = ifRequest ((== methodPost) . requestMethod) $ \app req resp -> do
  case getCookiedXsrfToken (cookieName cfg) req of
    Nothing ->
      app req resp
    Just token -> do
      app (req { requestHeaders = (headerName cfg, token) : requestHeaders req })
          resp

isPostFormReq :: Request -> Bool
isPostFormReq req
  = requestMethod req == methodPost &&
    lookup hContentType (requestHeaders req) == Just "application/x-www-form-urlencoded"

getPostFormXsrfToken :: Config -> Middleware
getPostFormXsrfToken cfg app req resp
  = case (requestMethod req, lookup hContentType (requestHeaders req)) of
      (m, Just "application/x-www-form-urlencoded")
        | m == methodPost -> do
            params <- parseQuery . mconcat . LB.toChunks <$> lazyRequestBody req
            app req resp
      _ ->
        app req resp

getCookiedXsrfToken
  :: ByteString
  -> Request
  -> Maybe ByteString
getCookiedXsrfToken cookieName_ req
  = lookup cookieName_ =<<
    parseCookies <$> lookup "Cookie" (requestHeaders req)
