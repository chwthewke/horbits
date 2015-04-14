module Horbits.Main(main) where

import           Data.Binding.Simple
import           Data.IORef
import           Graphics.UI.Gtk

import           Horbits.UI.Model
import           Horbits.UI.UIMain

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    model <- uiModelNew :: IO (Source IORef UIModel)
    mainLayout model window
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI

