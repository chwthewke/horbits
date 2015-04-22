{-# LANGUAGE FlexibleContexts #-}

module Horbits.UI.UIMain where

import           Control.Applicative
import           Control.Lens                hiding (set)
import           Control.Monad               (void)
import           Control.Monad.IO.Class
import           Graphics.Rendering.OpenGL   as GL
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.OpenGL
import           Prelude                     hiding (mapM_)
import           System.Clock

import           Horbits.Data.Binding
import           Horbits.SolarSystem
import           Horbits.UI.BodyDetails
import           Horbits.UI.BodyList
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLBody
import           Horbits.UI.GL.GLOrbit
import           Horbits.UI.GL.GLSetup
import           Horbits.UI.GL.GLTextures
import           Horbits.UI.Model
import           Horbits.UI.TimeControl
import           Horbits.UI.VisibilityToggle

orbitsCanvas :: HasBinding v UIModel => v -> IO GLDrawingArea
orbitsCanvas model = do
    let cam = mapVarL modelCamera model
    canvas <- setupGLWithCamera 600 600 cam
    onGtkGLInit canvas $ do
        bodyTex <- bodyTexture
        onGtkGLDraw canvas $ do
            getTime Monotonic >>= (mapVarL modelTick model $=)
            drawCanvas bodyTex
    return canvas
  where
    drawCanvas tex = do
        m <- readVar model
        let cam = m ^. modelCamera
        let instant = m ^. modelTime
        drawOrbits instant bodyIds
        drawBodies tex cam instant bodyIds
        GL.flush
    bodyIds = [minBound..]


mkBodyList :: HasUpdate v UIModel UIModel => v -> IO ScrolledWindow
mkBodyList model =
    bodyListView <$> bodyListNew (mapVarS modelSelectedBody model) [bodiesTree] (PolicyNever, PolicyAutomatic)

mkBodyDetails :: Bindable v UIModel => v -> IO ScrolledWindow
mkBodyDetails model = bodyDetailsNew (mapVarL modelSelectedBody model) (PolicyNever, PolicyAutomatic)


visibilityButtons :: (WidgetClass w, WidgetClass w') => w -> w' -> IO HBox
visibilityButtons wl wd = do
    buttonBox <- hBoxNew False 2
    buttonList <- visibilityToggleButton "Celestial Bodies" wl
    containerAdd buttonBox buttonList
    buttonData <- visibilityToggleButton "Body Details" wd
    containerAdd buttonBox buttonData
    return buttonBox

mkTimeControl :: HasBinding v UIModel => v -> IO HBox
mkTimeControl model = timeControlWidgetEditable (mapVarL modelClock model)

mainLayout :: HasBinding v UIModel => v -> Window -> IO ()
mainLayout model win = do
    _ <- set win [ windowTitle := "Horbits" ]
    list <- mkBodyList model
    details <- mkBodyDetails model
    timeControl <- mkTimeControl model
    canvas <- orbitsCanvas model
    mainBox <- hBoxNew False 2
    containerAdd mainBox list
    containerAdd mainBox details
    canvasBox <- vBoxNew False 2
    boxPackStart canvasBox timeControl PackNatural 0
    boxPackEnd canvasBox canvas PackGrow 0
    containerAdd mainBox canvasBox
    outerBox <- vBoxNew False 2
    boxPackStart outerBox mainBox PackRepel 0
    visibilityBar <- visibilityButtons list details
    boxPackEnd outerBox visibilityBar PackNatural 0
    containerAdd win outerBox
    void $ on win keyPressEvent $ tryEvent $ do
        [Control] <- eventModifier
        "k" <- eventKeyName
        liftIO $ logCamera (mapVarG modelCamera model)

