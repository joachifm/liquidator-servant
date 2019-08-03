{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

{-|
Module: Wai.Middleware.VerifyCsrToken

<https://github.com/OWASP/CheatSheetSeries/blob/master/cheatsheets/Cross-Site_Request_Forgery_Prevention_Cheat_Sheet.md>
-}

module Wai.Middleware.VerifyCsrToken
  ( Config(..)
  , defaultConfig
  , verifyXsrfToken
  , doubleSubmitCookie
  , cookieToHeader
  ) where

import Control.Monad (join)

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

import Data.IORef

import Network.HTTP.Types (parseQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.Wai
import Web.Cookie (parseCookies)

data Config = Config
  { cCookieName :: ByteString -- ^ Name of cookie carrying the XSRF token
  , cHeaderName :: HeaderName -- ^ Name of header holding XSRF token
  , cFieldName :: ByteString  -- ^ Name of input field used to hold XSRF token in forms
  }
  deriving (Show)

defaultConfig :: Config
defaultConfig = Config
  { cCookieName = "XSRF-TOKEN"
  , cHeaderName = "X-XSRF-TOKEN"
  , cFieldName = "xsrf_token"
  }

Config cookieName headerName fieldName = defaultConfig

-- |
-- Check that session token matches header token.
verifyXsrfToken :: Middleware
verifyXsrfToken app req rsp
  | requestMethod req == methodPost
  , Just cXsrf <- getXsrfTokenFromCookie req
  , Just hXsrf <- getXsrfTokenFromHeader req
  = do
      app req rsp
  | otherwise = app req rsp

-- |
-- A middleware that implements the "double submit cookie" approach
-- to CSRF mitigation.
--
-- Here, the session token is compared to a token passed via a hidden form
-- input.
--
-- The security of this approach relies /crucially/ on the attacker not being
-- able to write cookies on our behalf.  All our subdomains must be HTTPS only
-- for this to actually help mitigate CSRF attacks.
doubleSubmitCookie :: Middleware
doubleSubmitCookie = ifRequest isPostFormReq $ \app req rsp -> do
  (body, req') <- getRequestBody req

  case lookup fieldName (parseQuery body) of
    Just (Just token) -> do
      -- TODO(joachifm) check session cookie vs. form token
      putStrLn "doubleSubmitCookie: Found hidden input token"
      app req' rsp

    Just Nothing -> do
      putStrLn "doubleSubmitCookie: Hidden field contains no value!"
      app req' rsp

    Nothing -> do
      putStrLn "doubleSubmitCookie: No hidden input token"
      app req' rsp

-- |
-- Automatically add session token to header.
--
-- TODO this is typically done via javascript on the client, the idea being
--   that the client will only execute js from the same domain
cookieToHeader :: Middleware
cookieToHeader app req rsp
  | requestMethod req == methodPost
  , Just cXsrf <- getXsrfTokenFromCookie req
  = app (req { requestHeaders = (headerName, cXsrf) : requestHeaders req }) rsp
  | otherwise = app req rsp

getRequestBody :: Request -> IO (ByteString, Request)
getRequestBody req = do
  -- Work around hidden state that prevents reading the body more than once
  -- See e.g., "wai-extra" Network.Wai.Middleware.MethodOverridePost.setPost
  body <- mconcat . LB.toChunks <$> lazyRequestBody req
  ref <- newIORef body
  let rb = atomicModifyIORef ref $ \bs -> (mempty, bs)
  return (body, req { requestBody = rb })

------------------------------------------------------------------------
-- Internal utils
------------------------------------------------------------------------

isPostFormReq :: Request -> Bool
isPostFormReq req
  = requestMethod req == methodPost &&
    lookup hContentType (requestHeaders req) == Just "application/x-www-form-urlencoded"

getXsrfTokenFromCookie
  :: Request
  -> Maybe ByteString
getXsrfTokenFromCookie req
  = lookup cookieName =<<
    parseCookies <$> lookup "Cookie" (requestHeaders req)

getXsrfTokenFromHeader
  :: Request
  -> Maybe ByteString
getXsrfTokenFromHeader
  = lookup headerName . requestHeaders
