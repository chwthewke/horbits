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
timeControlWidgetEditable = undefined

--yearOf :: Prism' KerbalClock Integer
--yearOf = _StoppedClock . years
