{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import GHC.Generics (Generic)

import qualified Control.Exception as E
import Control.Monad.Except (ExceptT(ExceptT))

import Data.Text (Text)
import Data.Int (Int64)

import Data.Aeson (FromJSON, ToJSON)

import Servant

import Lucid (Html)
import Lucid.Html5
import Servant.HTML.Lucid

import Servant.Auth

import qualified Network.Wai.Handler.Warp as Warp

------------------------------------------------------------------------

convert
  :: IO a
  -> Handler a
convert
  = Handler . ExceptT . E.try

------------------------------------------------------------------------

data Secret = MkSecret
  { secretText :: !Text
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

type SecretApi
  = "secret" :> Get '[JSON] Secret

secretHandler
  :: ServerT SecretApi IO
secretHandler = pure $! MkSecret "hush"

------------------------------------------------------------------------

type Api = SecretApi

api :: Proxy Api
api = Proxy @Api

server :: Server Api
server = hoistServer api convert secretHandler

app :: Application
app = serve api server

------------------------------------------------------------------------

run :: IO ()
run = Warp.run 3000 app
