module Horbits.UI.GL.GLCamera(setGLCamera, bindCameraToGL) where

import           Control.Lens
import           Data.Binding.Simple
import           Graphics.Rendering.OpenGL
import           Horbits.UI.Camera
import           Linear

bindCameraToGL :: (RealFloat a, Epsilon a, Variable v) => Source v (OrthoCamera a) -> IO(Int -> Int -> IO ())
bindCameraToGL cam = do
    bind cam orthoCameraMatrix () (const setGLCamera)
    return resizeViewport
  where
    resizeViewport w h = modifyVar cam $
        set orthoCameraViewportWidth w . set orthoCameraViewportHeight h

setGLCamera :: (Real a) => M44 a -> IO ()
setGLCamera mat = do
    -- TODO test RowMajor/no transposing now that we have recent OpenGLRaw?
    m <- newMatrix ColumnMajor . fmap realToFrac $ transpose mat ^.. traverse.traverse :: IO (GLmatrix GLfloat)
    matrix (Just Projection) $= m
