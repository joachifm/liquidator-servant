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

import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LB

import Data.IORef

import Network.HTTP.Types (parseQuery)
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
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

cookieName, fieldName :: ByteString
headerName :: HeaderName
Config cookieName headerName fieldName = defaultConfig

-- | Check that session token matches header token.
verifyXsrfToken :: Middleware
verifyXsrfToken = ifRequest ((== methodPost) . requestMethod) $ \app req sendRsp -> do
  if getXsrfTokenFromCookie req == getXsrfTokenFromHeader req
    then app req sendRsp
    else sendRsp rejectedRsp

-- | An implementation of the "double submit cookie" CSRF mitigation.
--
-- Here, the session token is compared to a token passed via a hidden form
-- input.
--
-- A check is performed if the request is a form POST request.
--
-- The request is accepted if it contains an XSRF token /and/ that token
-- matches the session token.
--
-- The request is rejected if the form does not contain a token or the
-- token fails to match.
--
-- The security of this approach relies /crucially/ on the attacker not being
-- able to write cookies on our behalf.  At the very least, we must be able to
-- ensure that all subdomains are HTTPS only.
doubleSubmitCookie :: Middleware
doubleSubmitCookie = ifRequest isPostFormReq $ \app req sendRsp -> do
  (body, req') <- copyRequestBody req
  case lookup fieldName (parseQuery body) of
    Just (Just pXsrfToken) -> do
      case getXsrfTokenFromCookie req of
        Just cXsrfToken ->
          if cXsrfToken == pXsrfToken
          then app req' sendRsp -- Accepted!
          else do
            putStr "XSRF-TOKEN mismatch: "
            putStr "expected: " >> print cXsrfToken >> putStr "; "
            putStr "got: " >> print pXsrfToken
            putStrLn ""
            sendRsp rejectedRsp

        Nothing -> do
          putStrLn "No session XSRF token"
          sendRsp rejectedRsp

    Just Nothing -> do
      putStrLn "Request contains no XSRF token parameter"
      sendRsp rejectedRsp

    Nothing -> do
      putStrLn "Request contains no XSRF token parameter"
      sendRsp rejectedRsp

rejectedRsp :: Response
rejectedRsp
  = responseLBS status401
                [ (hContentType, "text/plain")
                , ("WWW-Authenticate", "Basic")
                ]
                "XSRF token mismatch"

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

-- | Read a copy of the request body.
copyRequestBody :: Request -> IO (ByteString, Request)
copyRequestBody req = do
  -- Work around hidden state that prevents reading the body more than once
  -- See e.g., "wai-extra" Network.Wai.Middleware.MethodOverridePost.setPost
  body <- mconcat . LB.toChunks <$> lazyRequestBody req
  ref <- newIORef body
  let rb = atomicModifyIORef ref $ \bs -> (mempty, bs)
  return (body, req { requestBody = rb })

------------------------------------------------------------------------
-- Internal utils
------------------------------------------------------------------------

-- | Check whether request is a POST form request.
isPostFormReq :: Request -> Bool
isPostFormReq req
  = requestMethod req == methodPost &&
    lookup hContentType (requestHeaders req) == Just "application/x-www-form-urlencoded"

-- | Extract session XSRF-TOKEN from cookie.
getXsrfTokenFromCookie
  :: Request
  -> Maybe ByteString
getXsrfTokenFromCookie req
  = lookup cookieName =<<
    parseCookies <$> lookup "Cookie" (requestHeaders req)

-- | Extract X-XSRF-TOKEN from request header.
getXsrfTokenFromHeader
  :: Request
  -> Maybe ByteString
getXsrfTokenFromHeader
  = lookup headerName . requestHeaders

extractCookie
  :: Request
  -> ByteString
  -> Maybe ByteString
extractCookie req name = lookup name
  =<< parseCookies <$> lookup hCookie (requestHeaders req)
