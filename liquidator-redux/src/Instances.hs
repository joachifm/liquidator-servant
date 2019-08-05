{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Instances () where

import Data.String (IsString(..))
import Data.Time.Calendar (Day)

instance IsString Day where fromString = read
