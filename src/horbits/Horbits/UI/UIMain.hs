module Horbits.UI.UIMain where

import           Control.Applicative
import           Control.Lens                 hiding (set)
import           Data.Binding.Simple
import           Data.Foldable
import           Graphics.Rendering.OpenGL    as GL
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.OpenGL
import           Prelude                      hiding (mapM_)

import           Horbits.Body
import           Horbits.Data.Variable.Mapped
import           Horbits.Dimensional.Prelude  (dim)
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
import           Horbits.UI.Model
import           Horbits.UI.VisibilityToggle

orbitsCanvas :: Bindable v => v UIModel -> IO GLDrawingArea
orbitsCanvas model = do
    let cam = mapVariable modelCamera model
    canvas <- setupGLWithCamera 600 600 cam
    onGtkGLInit canvas $ do
        bodyTex <- bodyTexture
        onGtkGLDraw canvas $ drawCanvas cam bodyTex
    -- TODO push into model? Not yet, might change with time-control and auto-follow
    bind (mapVariable modelSelectedBody model) id cam $ \c b -> forM_ b $ modifyVar c . lookAtBody
    return canvas
  where
    drawCanvas camera t = do
        cam <- readVar camera
        drawOrbits epoch bodyIds
        drawBodies t cam epoch bodyIds
        GL.flush
    lookAtBody body = orthoCameraCenter .~ (bodyPosition epoch (body ^. bodyId) ^. from dim)
    bodyIds = [minBound..]


bodyList :: Variable v => v UIModel -> IO ScrolledWindow
bodyList model =
    bodyListView <$> bodyListNew (mapVariable modelSelectedBody model) [bodiesTree] (PolicyNever, PolicyAutomatic)

bodyDetails :: Bindable v => v UIModel -> IO ScrolledWindow
bodyDetails model = bodyDetailsNew (mapVariable modelSelectedBody model) (PolicyNever, PolicyAutomatic)


visibilityButtons :: (WidgetClass w, WidgetClass w') => w -> w' -> IO HBox
visibilityButtons wl wd = do
    buttonBox <- hBoxNew False 2
    buttonList <- visibilityToggleButton "Celestial Bodies" wl
    containerAdd buttonBox buttonList
    buttonData <- visibilityToggleButton "Body Details" wd
    containerAdd buttonBox buttonData
    return buttonBox

mainLayout :: Bindable v => v UIModel -> Window -> IO ()
mainLayout model win = do
    _ <- set win [ windowTitle := "Horbits" ]
    list <- bodyList model
    details <- bodyDetails model
    canvas <- orbitsCanvas model
    mainBox <- hBoxNew False 2
    containerAdd mainBox list
    containerAdd mainBox details
    containerAdd mainBox canvas
    outerBox <- vBoxNew False 2
    containerAdd outerBox mainBox
    containerAdd outerBox =<< visibilityButtons list details
    containerAdd win outerBox
