{-# LANGUAGE OverloadedStrings #-}

module WebFront where

import Imports

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import qualified Control.Concurrent.Async as Async

import qualified Data.Text as Text

import Lucid (Html, toHtml)
import Lucid.Html5
import qualified Lucid.Base as Lucid

import Network.HTTP.Types.Status
import Web.Scotty hiding (html)

import IORef

import qualified WebFront.Views.IndexPage as IndexPage
import qualified WebFront.Views.CounterPage as CounterPage
import qualified WebFront.Views.NotFoundPage as NotFoundPage
import qualified WebFront.Views.SimplePage as SimplePage

------------------------------------------------------------------------

main :: IO ()
main = scotty 3000 =<< app <$> new

------------------------------------------------------------------------

data Handle = Handle
  { counter :: IORef Integer
  }

new
  :: IO Handle
new
  = Handle <$> newIORef 0

------------------------------------------------------------------------

app :: Handle -> ScottyM ()
app h = do
  get "/" $ do
    html $ IndexPage.render

  get "/counter" $ do
    cur <- liftIO $ postIncIORef (counter h)
    html $ CounterPage.render cur

  get "/new" $ do
    html $ SimplePage.render "New" $ do
      return ()

  get "/list" $ do
    html $ SimplePage.render "List" $ do
      return ()

  get "/view" $ do
    html $ SimplePage.render "View" $ do
      return ()

  get "/edit" $ do
    html $ SimplePage.render "Edit" $ do
      return ()

  notFound $ do
    html $ NotFoundPage.render

------------------------------------------------------------------------
-- Internal utils
------------------------------------------------------------------------

html :: Html () -> ActionM ()
html page = do
  setHeader "Content-Type" "text/html"
  text . Lucid.renderText $ doctypehtml_ page
