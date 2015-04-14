module Horbits.Main(main) where

import           Graphics.UI.Gtk

import           Horbits.UI.Model
import           Horbits.UI.UIMain

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    model <- uiModelNew
    mainLayout model window
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI

