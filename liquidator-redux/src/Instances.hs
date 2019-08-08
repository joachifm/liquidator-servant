{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Instances

Orphan instances
-}

module Instances () where

import Data.String (IsString(..))
import Data.Time.Calendar (Day)

------------------------------------------------------------------------
-- Day
------------------------------------------------------------------------

instance IsString Day where fromString = read
