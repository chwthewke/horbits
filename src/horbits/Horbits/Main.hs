

module Horbits.Main(main) where

import           Control.Applicative
import           Control.Lens                 hiding (set)
import           Data.Foldable                (forM_)
import           Data.Tree
import           Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Gtk
import           Numeric.Units.Dimensional.TF (Dimensional (Dimensional))

import           Horbits.Body
import           Horbits.Orbit
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLOrbit
import           Horbits.UI.GL.GLSetup

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
    (canvas, _) <- setupGLWithCamera 600 600 $ initOrthoCamera (geometricZoom 1.2 (1e6, 1e12))
    onGtkGLInit canvas $ onGtkGLDraw canvas drawCanvas
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


drawPlanetOrbit :: RgbaColor Float -> Orbit -> IO ()
drawPlanetOrbit col orbit = do
    let ce = orbit ^. centralOrbit
    let Dimensional c = ce ^. center
    let Dimensional a = ce ^. semiMajorAxisVector
    let Dimensional b = ce ^. semiMinorAxisVector
    drawEllipse3d col c a b

planets :: [BodyId]
planets = map (view bodyId . rootLabel) . subForest $ bodiesTree

planetOrbits :: [(RgbaColor Float, Orbit)]
planetOrbits = planets >>= getOrbit
  where
    getOrbit bId = (,) <$> (bId ^.. bodyUiColor) <*> (bId ^.. bodyOrbit)

drawCanvas :: IO ()
drawCanvas = do
    forM_ planetOrbits (uncurry drawPlanetOrbit)
    GL.flush

