module Main where

import           Control.Lens           hiding (set)
import           Graphics.UI.Gtk
import           Horbits.Body
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    _ <- set window [ windowTitle := "Hello" ]
    box <- hBoxNew True 5
    containerAdd window box
    bodyList <- bodyListNew bodiesTree
    containerAdd box $ bodyListView bodyList
    bodyDetailsPane <- bodyDetailsPaneNew
    _ <- bodyListOnSelectionChange bodyList (\b -> do
        putStrLn $ "Selected " ++ b ^. bodyName
        bodyDetailsPaneSetBody bodyDetailsPane b)
    containerAdd box $ bodyDetailsPaneView bodyDetailsPane
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI
