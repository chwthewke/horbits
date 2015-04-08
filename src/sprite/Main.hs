module Main where

import           Graphics.UI.Gtk
import           HorbitsSPR.SpritePointExploration

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    --
    spritePointWindow window
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI
