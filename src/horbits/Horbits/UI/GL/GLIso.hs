module Horbits.UI.GL.GLIso (glColor, glV3) where

import           Control.Lens
import           Foreign.C.Types
import           Graphics.Rendering.OpenGL.GL as GL
import           Linear.V3

import           Horbits.Body

glColor :: Iso' RgbaFColor (Color4 GLfloat)
glColor = iso toGlColor fromGlColor

toGlColor :: RgbaFColor -> Color4 GLfloat
toGlColor (RgbaFColor r g b a) = Color4 (CFloat r) (CFloat g) (CFloat b) (CFloat a)

fromGlColor :: Color4 GLfloat -> RgbaFColor
fromGlColor (Color4 (CFloat r) (CFloat g) (CFloat b) (CFloat a)) = RgbaFColor r g b a

glV3 :: Iso' (V3 Float) (Vertex3 GLfloat)
glV3 = iso v3ToGl glToV3

v3ToGl :: V3 Float -> Vertex3 GLfloat
v3ToGl (V3 x y z) = Vertex3 (CFloat x) (CFloat y) (CFloat z)

glToV3 :: Vertex3 GLfloat -> V3 Float
glToV3 (Vertex3 (CFloat x) (CFloat y) (CFloat z)) = V3 x y z
