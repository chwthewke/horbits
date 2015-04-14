module Horbits.Main(main) where

import           Graphics.UI.Gtk

import           Horbits.UI.UIMain

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    mainLayout window
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI

