module Horbits.UI.VisibilityToggle where

import           Graphics.UI.Gtk

visibilityToggleButton :: WidgetClass w => String -> w -> IO ToggleButton
visibilityToggleButton toggleText widget = do
    button <- toggleButtonNewWithLabel toggleText
    _ <- on button toggled $ set widget [ widgetVisible :~ not ]
    return button
