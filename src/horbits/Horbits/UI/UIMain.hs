module Horbits.UI.UIMain where

import           Control.Lens                hiding (set)
import           Control.Monad               (void)
import           Data.Binding.Simple
import           Data.Foldable
import           Graphics.Rendering.OpenGL   as GL
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.OpenGL
import           Prelude                     hiding (mapM_)

import           Horbits.Body
import           Horbits.Dimensional.Prelude (dim)
import           Horbits.KerbalDateTime
import           Horbits.Orbit
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLBody
import           Horbits.UI.GL.GLOrbit
import           Horbits.UI.GL.GLSetup
import           Horbits.UI.GL.GLTextures
import           Horbits.UI.VisibilityToggle

orbitsCanvas :: BodyList -> IO GLDrawingArea
orbitsCanvas (BodyList _ _ onBodySelection) = do
    (canvas, cam) <- setupGLWithCamera 600 600 $ initOrthoCamera (geometricZoom 1.2 (1e6, 1e12))
    onGtkGLInit canvas $ do
        bodyTex <- bodyTexture
        onGtkGLDraw canvas $ drawCanvas cam bodyTex
    void $ onBodySelection . mapM_ $ \b ->
        modifyVar cam $ orthoCameraCenter .~ (bodyPosition epoch (b ^. bodyId) ^. from dim)
    return canvas

drawCanvas :: Variable v => v (OrthoCamera Double) -> TextureObject -> IO ()
drawCanvas camera t = do
    cam <- readVar camera
    drawOrbits epoch bodyIds
    drawBodies t cam epoch bodyIds
    GL.flush
  where
    bodyIds = [minBound..]


bodyList :: IO BodyList
bodyList = bodyListNew [bodiesTree] (PolicyNever, PolicyAutomatic)


bodyDetails :: BodyList -> IO BodyDetails
bodyDetails l = do
    result <- bodyDetailsNew (PolicyNever, PolicyAutomatic)
    void $ bodyListOnSelectionChange l $ mapM_ (bodyDetailsSetBody result)
    return result

visibilityButtons :: BodyList -> BodyDetails -> IO HBox
visibilityButtons (BodyList wl _ _) (BodyDetails wd _) = do
    buttonBox <- hBoxNew False 2
    buttonList <- visibilityToggleButton "Celestial Bodies" wl
    containerAdd buttonBox buttonList
    buttonData <- visibilityToggleButton "Body Details" wd
    containerAdd buttonBox buttonData
    return buttonBox

mainLayout :: Window -> IO ()
mainLayout win = do
    _ <- set win [ windowTitle := "Horbits" ]
    list <- bodyList
    details <- bodyDetails list
    canvas <- orbitsCanvas list
    mainBox <- hBoxNew False 2
    containerAdd mainBox $ bodyListView list
    containerAdd mainBox $ bodyDetailsView details
    containerAdd mainBox canvas
    outerBox <- vBoxNew False 2
    containerAdd outerBox mainBox
    containerAdd outerBox =<< visibilityButtons list details
    containerAdd win outerBox
