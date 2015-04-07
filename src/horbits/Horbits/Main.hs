

module Horbits.Main(main) where

import           Control.Applicative
import           Control.Lens                 hiding (set)
import           Data.Binding.Simple
import           Data.Foldable                (forM_)
import           Data.IORef
import           Data.Tree
import           Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Gtk
import           Horbits.Body
import           Horbits.Orbit
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList
import           Horbits.UI.Camera
import           Horbits.UI.Camera.Control
import           Horbits.UI.Camera.Zoom
import           Horbits.UI.GL.GLCamera
import           Horbits.UI.GL.GLOrbit
import           Horbits.UI.GL.GLSetup
import           Numeric.Units.Dimensional.TF (Dimensional (Dimensional))

main :: IO ()
main = do
    _ <- initGUI
    window <- windowNew
    layoutDisplay window
    --
    widgetShowAll window
    _ <- onDestroy window mainQuit
    mainGUI

layoutDisplay :: Window -> IO ()
layoutDisplay window = do
    _ <- set window [ windowTitle := "Hello" ]
    -- Actual layout
    box <- hBoxNew True 5
    containerAdd window box
    containerAdd box =<< bodyDataPane
    rBox <- vBoxNew False 5
    containerAdd box rBox
    -- TODO four calls, really?
    camera <- newVar $ orthoCamera (geometricZoom 1.2 (1e6, 1e12)) 600 600 :: IO (Source IORef (OrthoCamera Double))
    resizeCb <- bindCameraToGL camera
    canvas <- setupGL 600 600 resizeCb drawCanvas
    _ <- setupMouseControl canvas camera
    containerAdd rBox canvas


bodyDataPane :: IO VPaned
bodyDataPane = do
    -- body list
    bodyList <- bodyListNew [bodiesTree]
    -- made vertically scrollable
    bodyListScroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy bodyListScroll PolicyNever PolicyAutomatic
    containerAdd bodyListScroll $ bodyListView bodyList
    -- body details pane
    bodyDetailsPane <- bodyDetailsPaneNew
    -- also made vertically scrollable
    bodyDetailsScroll <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy bodyDetailsScroll PolicyNever PolicyAutomatic
    scrolledWindowAddWithViewport bodyDetailsScroll $ bodyDetailsPaneView bodyDetailsPane
    -- reacts to body selection
    _ <- bodyListOnSelectionChange bodyList (bodyDetailsPaneSetBody bodyDetailsPane)
    -- Stack them in a VPaned
    pane <- vPanedNew
    panedAdd1 pane bodyListScroll
    panedAdd2 pane bodyDetailsScroll
    return pane


drawPlanetOrbit :: RgbaFColor -> ClassicalOrbit -> IO ()
drawPlanetOrbit col orbit = do
    let ce = orbit ^. centralOrbit
    let Dimensional c = ce ^. center
    let Dimensional a = ce ^. semiMajorAxisVector
    let Dimensional b = ce ^. semiMinorAxisVector
    drawEllipse3d col c a b

planets :: [BodyId]
planets = map (view bodyId . rootLabel) . subForest $ bodiesTree

planetOrbits :: [(RgbaFColor, ClassicalOrbit)]
planetOrbits = planets >>= getOrbit
  where
    getOrbit bId = (,) <$> (bId ^.. bodyUiColor) <*> (bId ^.. bodyOrbit)

drawCanvas :: IO ()
drawCanvas = do
    forM_ planetOrbits (uncurry drawPlanetOrbit)
    GL.flush

