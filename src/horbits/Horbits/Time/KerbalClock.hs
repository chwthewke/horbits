{-# LANGUAGE TemplateHaskell #-}

module Horbits.Time.KerbalClock(
    KerbalClock(..), _StoppedClock, _RunningClock,
    readClock, stopClock, startClock) where

import           Control.Lens
import           Linear
import           Linear.Affine
import           System.Clock

import           Horbits.Time.KerbalDateTime

data TimeFunction = TimeFunction { _timeFunctionBase  :: KerbalInstant
                                 , _timeFunctionRate  :: Double
                                 , _timeFunctionStart :: TimeSpec
                                 } deriving (Show, Eq)

data KerbalClock = StoppedClock KerbalInstant
                 | RunningClock TimeFunction
                 deriving (Show, Eq)

makePrisms ''KerbalClock

runTimeFunction :: TimeFunction -> TimeSpec -> KerbalInstant
runTimeFunction (TimeFunction base rate start) now = base .+^ rate *^ view (from isoFracSeconds) (elapsed start now)
  where elapsed (TimeSpec s1 ns1) (TimeSpec s2 ns2) = fromIntegral (s2 - s1) + 1e-9 * fromIntegral (ns2 - ns1)

readClock :: KerbalClock -> TimeSpec -> KerbalInstant
readClock (StoppedClock i) = const i
readClock (RunningClock tf) = runTimeFunction tf

stopClock :: KerbalClock -> TimeSpec -> KerbalClock
stopClock (RunningClock tf) = StoppedClock . runTimeFunction tf
stopClock s = const s

startClock :: KerbalClock -> Double -> TimeSpec -> KerbalClock
startClock time rate now = RunningClock $ TimeFunction (readClock time now) rate now
