{-# LANGUAGE Rank2Types #-}

module Horbits.UI.TimeControl where

import           Control.Applicative
import           Control.Lens
import           Control.Monad        (void)
import           Graphics.UI.Gtk

import           Horbits.Data.Binding
import           Horbits.Time


timeControlWidget :: (HasBinding v KerbalClock) => v -> IO Widget
timeControlWidget = undefined

timeControlWidgetFrozen :: (HasBinding v KerbalClock) => v -> IO HBox
timeControlWidgetFrozen = undefined

timeControlWidgetEditable :: (HasBinding v KerbalClock) => v -> IO HBox
timeControlWidgetEditable clk = do
    box <- hBoxNew True 2
    labelNew (Just "Year") >>= containerAdd box
    newTimeSpinButton clk years (1, 2**16) >>= containerAdd box
    labelNew (Just "Day") >>= containerAdd box
    newTimeSpinButton clk days (1, 426) >>= containerAdd box
    hSeparatorNew >>= containerAdd box
    newTimeSpinButton clk hours (0, 5)  >>= containerAdd box
    labelNew (Just ":") >>= containerAdd box
    newTimeSpinButton clk minutes (0, 59)  >>= containerAdd box
    labelNew (Just ":") >>= containerAdd box
    newTimeSpinButton clk seconds (0, 59)  >>= containerAdd box
    return box

newTimeSpinButton :: (HasBinding v KerbalClock, Integral a, Eq a)
                  => v -> Lens' KerbalInstant a-> (Double, Double) -> IO SpinButton
newTimeSpinButton clk unit (minValue, maxValue) = do
    spin <- spinButtonNewWithRange minValue maxValue 1
    bindSpinButtonToClockUnit clk unit spin
    return spin

bindSpinButtonToClockUnit :: (HasBinding v KerbalClock, Integral a, Eq a) => v -> Lens' KerbalInstant a -> SpinButton -> IO ()
bindSpinButtonToClockUnit v l spin = do
    void $ bindEq clockUnit $ spinButtonSetValue spin . fromIntegral
    void $ onValueSpinned spin $ spinButtonGetValueAsInt spin <&> fromIntegral >>= (clockUnit $=)
  where
    clockUnit = mapVarL l $ mapVarP _StoppedClock epoch v

stoppedClock :: Getter KerbalClock KerbalInstant
stoppedClock = pre _StoppedClock . non epoch

setStoppedClock :: Setter' KerbalClock KerbalInstant
setStoppedClock f = fmap StoppedClock . maybe (pure epoch) f . preview _StoppedClock

ssc :: KerbalClock -> KerbalInstant -> KerbalClock
ssc clk inst = clk & _StoppedClock .~ inst

yearOf :: Traversal' KerbalClock Integer
yearOf = _StoppedClock . years

dayOf :: Traversal' KerbalClock Integer
dayOf = _StoppedClock . days

hourOf :: Traversal' KerbalClock Integer
hourOf = _StoppedClock . hours

minuteOf :: Traversal' KerbalClock Integer
minuteOf = _StoppedClock . minutes

secondsOf :: Traversal' KerbalClock Integer
secondsOf = _StoppedClock . seconds

secondFractionOf :: Traversal' KerbalClock Double
secondFractionOf = _StoppedClock . secondsFraction

--data EditableClockWidget {
--                         ,
--                         }
