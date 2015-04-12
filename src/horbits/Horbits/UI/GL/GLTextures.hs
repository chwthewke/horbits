module Horbits.UI.GL.GLTextures where

import           Control.Applicative
import           Graphics.GLUtil
import           Graphics.Rendering.OpenGL
import           Paths_horbits

bodyTexture :: IO TextureObject
bodyTexture = do
    fileName <- getDataFileName "data/body.png"
    tex <- either error id <$> readTexture fileName
    textureFilter Texture2D $= ((Linear', Nothing), Linear')
    texture2DWrap $= (Mirrored, ClampToEdge)
    return tex
