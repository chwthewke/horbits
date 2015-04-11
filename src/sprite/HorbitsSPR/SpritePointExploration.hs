module HorbitsSPR.SpritePointExploration where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binding.Simple
import           Data.List
import           Data.Ord
import           Graphics.GLUtil               (readTexture, texture2DWrap, withTextures2D)
import           Graphics.Rendering.OpenGL     as GL
import           Graphics.Rendering.OpenGL.Raw
import           Graphics.UI.Gtk
import           Linear
import           Paths_horbits

import           Horbits.UI.Camera
import           Horbits.UI.GL.GLSetup



spritePointWindow :: Window -> IO ()
spritePointWindow window = do
    (canvas, cam) <- setupGLWithCamera 600 600 $ initOrthoCamera (linearZoom 1 (1, 20))
    onGtkGLInit canvas $ do
        tex <- myTexture
        onGtkGLDraw canvas $ readVar cam >>= flip draw tex
    containerAdd window canvas

--readMyImage :: IO (JP.Image PixelRGB8)
--readMyImage = do
--    fileName <- getDataFileName "data/particle.bmp"
--    img <- readBitmap fileName
--    case img of Right (ImageRGB8 image) -> return image
--                Left s -> error $ "readImage error: " ++  s
--                _ -> error "Not an ImageRGB8"
---- TODO use ColorConvertible and RGBA8 ?
--
--
--initTexture :: IO TextureObject
--initTexture = do
--    glEnable gl_TEXTURE_2D
--    texObject <- genObjectName
--    textureBinding Texture2D $= Just texObject
--    JP.Image width height dat <- readMyImage
--    unsafeWith dat $ \ptr -> texImage2D Texture2D
--                                        NoProxy
--                                        0
--                                        RGB8
--                                        (TextureSize2D (fromIntegral width) (fromIntegral height))
--                                        0
--                                        (PixelData RGB UnsignedByte ptr)
--    return texObject

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
    withTextures2D [tex] .
        withStateVar (texture Texture2D) Enabled .
        withStateVar (textureBinding Texture2D) (Just tex) .
        withStateVar blend Enabled .
        withStateVar blendFunc (SrcAlpha, OneMinusSrcAlpha) .
        withStateVar depthFunc (Just Lequal) .
        -- TODO lots of parameters (point_fade_threshold, point_distance_attenuation, others?)
        withStateVar pointSize 64 $ do
            glTexEnvf gl_POINT_SPRITE_ARB gl_COORD_REPLACE_ARB (fromIntegral gl_TRUE)
            glEnable gl_POINT_SPRITE_ARB
            renderPrimitive Points $ forM_ (sortPoints ps) $ \(v, c) -> do
                color c
                vertex v
            glDisable gl_POINT_SPRITE_ARB
  where
    sortPoints = sortBy (comparing (negate . zIdx . fst))
    zIdx (Vertex3 x y z) = orthoCameraZIndex cam (realToFrac <$> V3 x y z) 


myTexture :: IO TextureObject
myTexture = do
    fileName <- getDataFileName "data/body.png"
    to <- either error id <$> readTexture fileName
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Mirrored, ClampToEdge)
    return to

