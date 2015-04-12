module HorbitsSPR.SpritePointExploration where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Lens
import           Data.Binding.Simple
import           Data.List
import           Data.Ord
import           Graphics.Rendering.OpenGL     as GL
import           Graphics.UI.Gtk

import           Horbits.Graphics.Rendering.OpenGL.PointSpriteARB
import           Horbits.UI.Camera
import           Horbits.UI.GL.GLIso
import           Horbits.UI.GL.GLSetup
import           Horbits.UI.GL.GLTextures



spritePointWindow :: Window -> IO ()
spritePointWindow window = do
    (canvas, cam) <- setupGLWithCamera 600 600 $ initOrthoCamera (linearZoom 1 (1, 20))
    onGtkGLInit canvas $ do
        tex <- bodyTexture
        onGtkGLDraw canvas $ readVar cam >>= flip draw tex
    containerAdd window canvas


withStateVar :: (HasGetter s a, HasSetter s a, MonadIO m) => s -> a -> m b -> m b
withStateVar st a b = do
    old <- liftIO $ GL.get st
    liftIO $ st $= a
    r <- b
    liftIO $ st $= old
    return r


draw :: OrthoCamera Double -> TextureObject -> IO ()
draw cam tex = do
    drawSimplex $ Vertex4 0 0 0 1
    drawSprites cam tex [ (Vertex3 0 0 (-1), Color3 0 0.8 0.8),
                          (Vertex3 0 (-1) 0, Color3 0.8 0 0.8),
                          (Vertex3 (-1) 0 0, Color3 0.8 0.8 0),
                          (Vertex3 1 1 1, Color3 0.5 0.5 0.5) ]

drawSimplex :: Vertex4 GLdouble -> IO ()
drawSimplex (Vertex4 x y z w) =
    let (o, a, b, c) = ( Vertex3 x y z
                       , Vertex3 (x+w) y z
                       , Vertex3 x (y+w) z
                       , Vertex3 x y (z+w)
                       ) in
        renderPrimitive Triangles $ do
            color (Color3 1 0 0 :: Color3 GLfloat)
            vertex o
            vertex a
            vertex b
            color (Color3 0 1 0 :: Color3 GLfloat)
            vertex o
            vertex a
            vertex c
            color (Color3 0 0 1 :: Color3 GLfloat)
            vertex o
            vertex b
            vertex c
            color (Color3 1 1 1 :: Color3 GLfloat)
            vertex a
            vertex b
            vertex c


drawSprites :: OrthoCamera Double -> TextureObject -> [(Vertex3 GLdouble, Color3 GLdouble)] -> IO ()
drawSprites cam tex ps = 
    withStateVar (texture Texture2D) Enabled .
    withStateVar (textureBinding Texture2D) (Just tex) .
    withStateVar blend Enabled .
    withStateVar blendFunc (SrcAlpha, OneMinusSrcAlpha) .
    withStateVar depthFunc (Just Lequal) .
    withStateVar pointSpriteARB Enabled .
    -- TODO lots of parameters (point_fade_threshold, point_distance_attenuation, others?)
    withStateVar pointSize 64 $ 
        renderPrimitive Points $ forM_ (sortPoints ps) $ \(v, c) -> do
            color c
            vertex v
  where
    sortPoints = sortBy (comparing (zIdx . fst))
    zIdx v = orthoCameraZIndex cam (v ^. from glV3) 



