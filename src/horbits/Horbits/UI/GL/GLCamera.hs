{-# LANGUAGE FlexibleContexts #-}

module Horbits.UI.GL.GLCamera(setGLCamera, bindCameraToGL) where

import           Control.Lens
import           Control.Monad             (void)
import           Graphics.Rendering.OpenGL
import           Graphics.UI.Gtk.OpenGL
import           Linear

import           Horbits.Data.Binding
import           Horbits.UI.Camera

bindCameraToGL :: (RealFloat a, Epsilon a, Show a, HasBinding v (OrthoCamera a))
               => GLDrawingArea -> v -> IO(Int -> Int -> IO ())
bindCameraToGL gld cam = do
    void $ bindEq (mapVar orthoCameraMatrix cam) $ setGLCamera gld
    return resizeViewport
  where
    resizeViewport w h = cam $~
        set orthoCameraViewportWidth w . set orthoCameraViewportHeight h

setGLCamera :: Real a => GLDrawingArea -> M44 a -> IO ()
setGLCamera gld mat = withGLDrawingArea gld . const $ do
    m <- newMatrix RowMajor . fmap realToFrac $ mat ^.. traverse.traverse :: IO (GLmatrix GLfloat)
    matrix (Just Projection) $= m
