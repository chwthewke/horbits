module Horbits.UI.GL.GLIso (glColor, glV3) where

import           Control.Lens
import           Data.Word
import           Foreign.C.Types
import           Graphics.Rendering.OpenGL.GL as GL
import           Horbits.Body
import           Linear.V3

glColor :: Iso' Rgb8Color (Color3 GLfloat)
glColor = iso toGlColor fromGlColor

toGlColor :: Rgb8Color -> Color3 GLfloat
toGlColor (Rgb8Color r g b) = Color3 (w8ToF r) (w8ToF g) (w8ToF b)

fromGlColor :: Color3 GLfloat -> Rgb8Color
fromGlColor (Color3 r g b) = Rgb8Color (fToW8 r) (fToW8 g) (fToW8 b)

w8ToF :: Word8 -> GLfloat
w8ToF w = fromIntegral w / 256

fToW8 :: GLfloat -> Word8
fToW8 f = truncate $ f * 256

glV3 :: Iso' (V3 Float) (Vertex3 GLfloat)
glV3 = iso v3ToGl glToV3

v3ToGl :: V3 Float -> Vertex3 GLfloat
v3ToGl (V3 x y z) = Vertex3 (CFloat x) (CFloat y) (CFloat z)

glToV3 :: Vertex3 GLfloat -> V3 Float
glToV3 (Vertex3 (CFloat x) (CFloat y) (CFloat z)) = V3 x y z
