module Horbits.UI.GL.GLSetup(setupGL) where

import           Control.Monad.IO.Class
import           Graphics.Rendering.OpenGL
import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.OpenGL

-- | Setup a GL canvas from a resize callback and a draw loop
-- | TODO Rather monolithic, maybe builders? in CPS?
setupGL :: Int -- ^ Initial width request
        -> Int -- ^ Initial height request
        -> (Int -> Int -> IO ()) -- ^ resize
        -> IO () -- ^ Draw frame
        -> IO GLDrawingArea
setupGL w h resizeCb draw = do
    glConfig <- glConfigNew [GLModeRGBA, GLModeDepth, GLModeDouble]
    canvas <- glDrawingAreaNew glConfig
    widgetSetSizeRequest canvas w h
    _ <- on canvas realize . withGLDrawingArea canvas . const $ doInit
    _ <- on canvas sizeAllocate $ \(Rectangle _ _ width height) -> doResize width height
    _ <- on canvas exposeEvent . tryEvent . liftIO $ withGLDrawingArea canvas doDraw
    _ <- timeoutAdd (widgetQueueDraw canvas >> return True) (1000 `div` 60)
    return canvas
  where
    doInit = do
        clearColor $= Color4 0.0 0.0 0.0 0.0
        depthFunc $= Just Less
        drawBuffer $= BackBuffers
        doResize w h
    doResize width height = do
        viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
        resizeCb width height
    doDraw glWin = do
        clear [DepthBuffer, ColorBuffer]
        draw
        glDrawableSwapBuffers glWin
