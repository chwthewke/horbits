module Horbits.UI.TimeControl where

import           Control.Lens
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
    spinButtonNewWithRange 1 (2**16) 1 >>= containerAdd box
    spinButtonNewWithRange 1 426 1 >>= containerAdd box
    spinButtonNewWithRange 0 5 1 >>= containerAdd box
    spinButtonNewWithRange 0 59 1 >>= containerAdd box
    spinButtonNewWithRange 0 59 1 >>= containerAdd box
    spinButtonNewWithRange 0 999 1 >>= containerAdd box
    return box

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
