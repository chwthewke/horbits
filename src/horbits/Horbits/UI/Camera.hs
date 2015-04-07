{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE TemplateHaskell #-}


module Horbits.UI.Camera(OrthoCamera(..), orthoCameraNew, orthoCameraMatrix,
    orthoCameraCenter, orthoCameraColatitude, orthoCameraLongitude,
    orthoCameraScale, orthoCameraViewportHeight, orthoCameraViewportWidth,
    addColatitude, addLongitude, addTranslation)
  where

import           Control.Lens
import           Data.Binding.Simple
import           Data.Fixed
import           Data.IORef
import           Linear

data OrthoCamera a = OrthoCamera { _orthoCameraCenter         :: V3 a
                                 , _orthoCameraColatitude     :: a
                                 , _orthoCameraLongitude      :: a
                                 , _orthoCameraScale          :: a
                                 , _orthoCameraViewportWidth  :: Int
                                 , _orthoCameraViewportHeight :: Int
                                 } deriving (Show, Eq, Functor)

makeLenses ''OrthoCamera

orthoCameraAspectRatio :: (RealFloat a) => OrthoCamera a -> a
orthoCameraAspectRatio cam =
    realToFrac (cam ^. orthoCameraViewportWidth) / realToFrac (cam ^. orthoCameraViewportHeight)

orthoCameraMatrix :: (RealFloat a, Epsilon a) => OrthoCamera a -> M44 a
orthoCameraMatrix cam = rotateColat cam !*! rotateLong cam !*! scale cam !*! translate cam

invOrthoCameraMatrix :: (RealFloat a, Epsilon a) => OrthoCamera a -> M44 a
invOrthoCameraMatrix cam = invTranslate cam !*! invScale cam !*! invRotateLong cam !*! invRotateColat cam

-- TODO linear has an ortho matrix that does scale + transl, use it?

-- update API

addColatitude :: RealFloat a => a -> OrthoCamera a -> OrthoCamera a
addColatitude a cam = cam & orthoCameraColatitude %~ addClamped
    where addClamped b = min pi $ max 0 $ a + b

addLongitude :: RealFloat a => a -> OrthoCamera a -> OrthoCamera a
addLongitude a cam = cam & orthoCameraLongitude %~ addWrapped
    where addWrapped b = mod' (a + b) (2 * pi)

addTranslation :: (RealFloat a, Epsilon a) => V2 a -> OrthoCamera a -> OrthoCamera a
addTranslation v cam = cam & orthoCameraCenter %~ (^-^ v')
    where v' = (invOrthoCameraMatrix cam !* (zero & _xy .~ v)) ^. _xyz

-- transformation parts

translate :: Num a => OrthoCamera a -> M44 a
translate = translate' . negate . view orthoCameraCenter

scale :: RealFloat a => OrthoCamera a -> M44 a
scale cam = scale' (cam ^. orthoCameraScale) (orthoCameraAspectRatio cam)

rotateLong :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
rotateLong = rotateZ . view orthoCameraLongitude

rotateColat :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
rotateColat = rotateX . view orthoCameraColatitude

-- inverse transformation parts

invTranslate :: Num a => OrthoCamera a -> M44 a
invTranslate = translate' . view orthoCameraCenter

invScale :: RealFloat a => OrthoCamera a -> M44 a
invScale cam = scale' (1 / cam ^. orthoCameraScale) (1 / orthoCameraAspectRatio cam)

invRotateLong :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
invRotateLong = rotateZ . negate . view orthoCameraLongitude

invRotateColat :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
invRotateColat = rotateX . negate . view orthoCameraColatitude


-- primitive transformations

rotateZ :: (Epsilon a, Floating a) => a -> M44 a
rotateZ a = mkTransformation (axisAngle (V3 0 0 1) a) zero

rotateX :: (Epsilon a, Floating a) => a -> M44 a
rotateX a = mkTransformation (axisAngle (V3 1 0 0) a) zero

scale' :: (RealFloat a) => a -> a -> M44 a
scale' sz rat =
    scaled $ V4 0 0 0 1 & _xyz .~ (1/sz) *^ ratioScale
  where
    ratioScale = if rat > 1 then V3 (1/rat) 1 (-1) else V3 1 rat (-1)

translate' :: (Num a) => V3 a -> M44 a
translate' v = identity & column _w . _xyz .~ v


-- TODO does this even belong here?
orthoCameraNew :: RealFloat a => a -> Int -> Int -> IO (Source IORef (OrthoCamera a))
orthoCameraNew sz w h = newVar $ OrthoCamera zero 0 0 sz w h

