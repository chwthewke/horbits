module Horbits.Main(main) where

import           Control.Applicative
import           Control.Lens                 hiding (set)
import           Data.Tree
import           Data.Variable
import           Graphics.Rendering.OpenGL.GL as GL
import           Graphics.UI.Gtk

import           Horbits.Body
import           Horbits.KerbalDateTime
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLBody
import           Horbits.UI.GL.GLOrbit
import           Horbits.UI.GL.GLSetup
import           Horbits.UI.GL.GLTextures
import           Horbits.UI.VisibilityToggle

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
    _ <- set window [ windowTitle := "Horbits" ]
    -- Actual layout
    -- Halp! Drowning in boxes
    outerBox <- vBoxNew False 2
    containerAdd window outerBox
    mainBox <- hBoxNew False 2
    containerAdd outerBox mainBox
    (list, details) <- bodyDataPane
    containerAdd mainBox list
    containerAdd mainBox details
    (canvas, cam) <- setupGLWithCamera 600 600 $ initOrthoCamera (geometricZoom 1.2 (1e6, 1e12))
    onGtkGLInit canvas $ do
        bodyTex <- bodyTexture
        onGtkGLDraw canvas $ drawCanvas cam bodyTex
    containerAdd mainBox canvas
    buttonBox <- hBoxNew False 2
    containerAdd outerBox buttonBox
    buttonList <- visibilityToggleButton "Celestial Bodies" list
    containerAdd buttonBox buttonList
    buttonData <- visibilityToggleButton "Body Details" details
    containerAdd buttonBox buttonData



bodyDataPane :: IO (ScrolledWindow, ScrolledWindow)
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
    return (bodyListScroll, bodyDetailsScroll)


planets :: [BodyId]
planets = map (view bodyId . rootLabel) . subForest $ bodiesTree

drawCanvas :: Variable v => v (OrthoCamera Double) -> TextureObject -> IO ()
drawCanvas camera t = do
    cam <- readVar camera
    drawOrbits planets
    drawBodies t (toGlBody cam <$> Sun : planets)
    GL.flush
  where toGlBody cam = drawBodySpec cam epoch

