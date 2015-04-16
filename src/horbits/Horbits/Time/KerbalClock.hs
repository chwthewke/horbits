
module Horbits.Time.KerbalClock where

import           Control.Lens
import           Linear
import           Linear.Affine
import           System.Clock

import           Horbits.Time.KerbalDateTime

data TimeFunction = TimeFunction { _runningTimeBase  :: KerbalInstant
                                 , _runningTimeRate  :: Double
                                 , _runningTimeStart :: TimeSpec
                                 }


data KerbalClock = StoppedClock KerbalInstant
                 | RunningClock TimeFunction



runTimeFunction :: TimeFunction -> TimeSpec -> KerbalInstant
runTimeFunction (TimeFunction base rate start) now = base .+^ rate *^ view (from isoFracSeconds) (elapsed start now)
  where elapsed (TimeSpec s1 ns1) (TimeSpec s2 ns2) = fromIntegral (s2 - s1) + 1e-9 * fromIntegral (ns2 - ns1)

evalTimeModel :: KerbalClock -> TimeSpec -> KerbalInstant
evalTimeModel (StoppedClock i) = const i
evalTimeModel (RunningClock tf) = runTimeFunction tf

stopClock :: KerbalClock -> TimeSpec -> KerbalClock
stopClock (RunningClock tf) = StoppedClock . runTimeFunction tf
stopClock s = const s

startClock :: KerbalClock -> Double -> TimeSpec -> KerbalClock
startClock time rate now = RunningClock $ TimeFunction (evalTimeModel time now) rate now
