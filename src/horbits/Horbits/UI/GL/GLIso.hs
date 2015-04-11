{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Horbits.UI.GL.GLIso (glColorF, glVector, glV1, glV2, glV3, glV4) where

import           Control.Lens
import           Data.Int
import           Data.Word
import           Foreign.C.Types
import           Graphics.Rendering.OpenGL.GL as GL
import           Linear

import           Horbits.Body

glColorF :: Iso' (RgbaColor Float) (Color4 GLfloat)
glColorF = glVector

glV1 :: (GLNumIso a b) => Iso' (V1 a) (Vertex1 b)
glV1 = glVector
glV2 :: (GLNumIso a b) => Iso' (V2 a) (Vertex2 b)
glV2 = glVector
glV3 :: (GLNumIso a b) => Iso' (V3 a) (Vertex3 b)
glV3 = glVector
glV4 :: (GLNumIso a b) => Iso' (V4 a) (Vertex4 b)
glV4 = glVector

class GLNumIso n g where
    glNumIso :: Iso' n g

instance GLNumIso Double GLdouble where
    glNumIso = iso CDouble (\(CDouble x) -> x)

instance GLNumIso Float GLfloat where
    glNumIso = iso CFloat (\(CFloat x) -> x)

instance GLNumIso Int8 GLbyte where
    glNumIso = iso CSChar (\(CSChar x) -> x)

instance GLNumIso Word8 GLubyte where
    glNumIso = iso CUChar (\(CUChar x) -> x)

instance GLNumIso Int16 GLshort where
    glNumIso = iso CShort (\(CShort x) -> x)

instance GLNumIso Word16 GLushort where
    glNumIso = iso CUShort (\(CUShort x) -> x)

instance GLNumIso Int32 GLint where
    glNumIso = iso CInt (\(CInt x) -> x)

instance GLNumIso Word32 GLuint where
    glNumIso = iso CUInt (\(CUInt x) -> x)

class Functor f => GLFunctorIso f g where
    glFunctorIso :: Iso' (f a) (g a)

instance GLFunctorIso V1 Vertex1 where
    glFunctorIso = iso (\(V1 x) -> Vertex1 x) (\(Vertex1 x) -> V1 x)

instance GLFunctorIso V2 Vertex2 where
    glFunctorIso = iso (\(V2 x y) -> Vertex2 x y) (\(Vertex2 x y) -> V2 x y)

instance GLFunctorIso V3 Vertex3 where
    glFunctorIso = iso (\(V3 x y z) -> Vertex3 x y z) (\(Vertex3 x y z) -> V3 x y z)

instance GLFunctorIso V4 Vertex4 where
    glFunctorIso = iso (\(V4 x y z w) -> Vertex4 x y z w) (\(Vertex4 x y z w) -> V4 x y z w)

instance GLFunctorIso RgbaColor Color4 where
    glFunctorIso = iso (\(RgbaColor r g b a) -> Color4 r g b a) (\(Color4 r g b a) -> RgbaColor r g b a)

glVector :: (GLNumIso a b, GLFunctorIso f g) => Iso' (f a) (g b)
glVector = mapping glNumIso . glFunctorIso
