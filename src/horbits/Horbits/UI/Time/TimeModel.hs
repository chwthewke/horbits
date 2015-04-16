{-# LANGUAGE TemplateHaskell #-}

module Horbits.UI.Time.TimeModel where

import           Control.Lens
import           Linear
import           Linear.Affine
import           System.Clock

import           Horbits.KerbalDateTime

data TimeFunction = TimeFunction { _runningTimeBase  :: KerbalInstant
                                 , _runningTimeRate  :: Double
                                 , _runningTimeStart :: TimeSpec
                                 }

makeLenses ''TimeFunction

data TimeModel = FixedTime KerbalInstant
               | RunningTime TimeFunction

makePrisms ''TimeModel


runTimeFunction :: TimeFunction -> TimeSpec -> KerbalInstant
runTimeFunction (TimeFunction base rate start) now = base .+^ rate *^ view (from isoFracSeconds) (elapsed start now)
  where elapsed (TimeSpec s1 ns1) (TimeSpec s2 ns2) = fromIntegral (s2 - s1) + 1e-9 * fromIntegral (ns2 - ns1)

evalTimeModel :: TimeModel -> TimeSpec -> KerbalInstant
evalTimeModel (FixedTime i) = const i
evalTimeModel (RunningTime tf) = runTimeFunction tf

fixedTimeModel :: KerbalInstant -> TimeModel
fixedTimeModel = FixedTime

startTimeModel :: TimeModel -> KerbalInstant -> Double -> TimeSpec -> TimeModel
startTimeModel = undefined
