module Main where

import           Graphics.UI.Gtk
import           HorbitsCC.CameraControlExploration

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    basesAndCameraControls window
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI

