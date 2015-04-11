module Horbits.UI.GL.GLCamera(setGLCamera, bindCameraToGL) where

import           Control.Lens
import           Data.Binding.Simple
import           Graphics.Rendering.OpenGL
import           Graphics.UI.Gtk.OpenGL
import           Linear

import           Horbits.UI.Camera

bindCameraToGL :: (RealFloat a, Epsilon a, Variable v) =>
                    GLDrawingArea -> Source v (OrthoCamera a) -> IO(Int -> Int -> IO ())
bindCameraToGL gld cam = do
    bind cam orthoCameraMatrix () $ const $ setGLCamera gld
    return resizeViewport
  where
    resizeViewport w h = modifyVar cam $
        set orthoCameraViewportWidth w . set orthoCameraViewportHeight h

setGLCamera :: (Real a) => GLDrawingArea -> M44 a -> IO ()
setGLCamera gld mat = withGLDrawingArea gld . const $ do
    m <- newMatrix RowMajor . fmap realToFrac $ mat ^.. traverse.traverse :: IO (GLmatrix GLfloat)
    matrix (Just Projection) $= m
