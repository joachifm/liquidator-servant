{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances where

import Data.String
import Data.Time.Calendar

instance IsString Day where fromString = read
