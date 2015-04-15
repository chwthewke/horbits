{-# LANGUAGE TemplateHaskell #-}

module Horbits.UI.Time.TimeModel where

import           Control.Lens
import           System.Clock

import           Horbits.KerbalDateTime

data TimeModel = TimeModel { _timeModelCurrent    :: KerbalInstant
                           , _timeModelRunning    :: Integer
                           , _timeModelLastUpdate :: TimeSpec
                           }

makeLenses ''TimeModel

