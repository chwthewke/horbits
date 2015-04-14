{-# LANGUAGE Rank2Types #-}

module Horbits.UI.GL.GLSetup(setupGLWithCamera, onGtkGLInit, onGtkGLDraw) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binding.Simple
import           Graphics.Rendering.OpenGL
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.OpenGL

import           Horbits.Data.Variable.State
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLCamera

-- | Setup a GL canvas with mouse-controlled ortho camera
setupGLWithCamera :: Bindable v
                  => Int -- ^ Initial width request
                  -> Int -- ^ Initial height request
                  -> v (OrthoCamera Double) -- ^ camera source
                  -> IO GLDrawingArea
setupGLWithCamera width height camera = do
    runStateVar camera $ do
        orthoCameraViewportWidth .= width
        orthoCameraViewportHeight .= height
    canvas <- glDrawingAreaNew =<< glConfigNew [GLModeRGBA, GLModeDepth, GLModeDouble]
    widgetSetSizeRequest canvas width height
    void $ on canvas realize . withGLDrawingArea canvas . const $ do
        clearColor $= Color4 0.0 0.0 0.0 0.0
        depthFunc $= Just Less
        drawBuffer $= BackBuffers
        resizeCb <- bindCameraToGL canvas camera
        void $ on canvas sizeAllocate $ \(Rectangle _ _ w h) -> do
            viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
            resizeCb w h
    void $ setupMouseControl canvas camera
    void $ timeoutAdd (widgetQueueDraw canvas >> return True) (1000 `div` 60)
    return canvas


onGtkGLInit :: GLDrawingArea -> IO () -> IO ()
onGtkGLInit canvas = void . on canvas realize . withGLDrawingArea canvas . const

onGtkGLDraw :: GLDrawingArea -> IO () -> IO ()
onGtkGLDraw canvas drawAction =
    void . on canvas exposeEvent . tryEvent . liftIO . withGLDrawingArea canvas $ \glWin -> do
        clear [DepthBuffer, ColorBuffer]
        drawAction
        glDrawableSwapBuffers glWin
