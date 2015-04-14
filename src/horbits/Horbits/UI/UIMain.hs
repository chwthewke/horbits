{-# LANGUAGE FlexibleContexts #-}

module Horbits.UI.UIMain where

import           Control.Applicative
import           Control.Lens                hiding (set)
import           Control.Monad               (void)
import           Data.Foldable
import           Graphics.Rendering.OpenGL   as GL
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.OpenGL
import           Prelude                     hiding (mapM_)

import           Horbits.Body
import           Horbits.Data.Binding
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
import           Horbits.UI.Model
import           Horbits.UI.VisibilityToggle

orbitsCanvas :: HasBinding v UIModel => v -> IO GLDrawingArea
orbitsCanvas model = do
    let cam = mapVarL modelCamera model
    canvas <- setupGLWithCamera 600 600 cam
    onGtkGLInit canvas $ do
        bodyTex <- bodyTexture
        onGtkGLDraw canvas $ drawCanvas cam bodyTex
    -- TODO push into model? Not yet, might change with time-control and auto-follow
    void $ bindEq (mapVarG modelSelectedBody model) $ \b -> forM_ b $ \b' -> cam $~ lookAtBody b'
    return canvas
  where
    drawCanvas camera t = do
        cam <- readVar camera
        drawOrbits epoch bodyIds
        drawBodies t cam epoch bodyIds
        GL.flush
    lookAtBody body = orthoCameraCenter .~ (bodyPosition epoch (body ^. bodyId) ^. from dim)
    bodyIds = [minBound..]


bodyList :: HasUpdate v UIModel UIModel => v -> IO ScrolledWindow
bodyList model =
    bodyListView <$> bodyListNew (mapVarS modelSelectedBody model) [bodiesTree] (PolicyNever, PolicyAutomatic)

bodyDetails :: Bindable v UIModel => v -> IO ScrolledWindow
bodyDetails model = bodyDetailsNew (mapVarL modelSelectedBody model) (PolicyNever, PolicyAutomatic)


visibilityButtons :: (WidgetClass w, WidgetClass w') => w -> w' -> IO HBox
visibilityButtons wl wd = do
    buttonBox <- hBoxNew False 2
    buttonList <- visibilityToggleButton "Celestial Bodies" wl
    containerAdd buttonBox buttonList
    buttonData <- visibilityToggleButton "Body Details" wd
    containerAdd buttonBox buttonData
    return buttonBox

mainLayout :: HasBinding v UIModel => v -> Window -> IO ()
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
