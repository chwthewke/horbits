{-# LANGUAGE TemplateHaskell #-}

module Horbits.UI.Camera(OrthoCamera(..), orthoCamera, orthoCameraMatrix,
    orthoCameraCenter, orthoCameraColatitude, orthoCameraLongitude, orthoCameraScale,
    orthoCameraViewportHeight, orthoCameraViewportWidth, orthoCameraZoomModel,
    addColatitude, addLongitude, addTranslation, zoomIn, zoomOut,
    scale) -- TODO scale exposed for debugging
  where

import           Control.Lens
import           Data.Fixed
import           Data.List.NonEmpty     as NE
import           Horbits.UI.Camera.Zoom
import           Linear

-- data type

data OrthoCamera a = OrthoCamera { _orthoCameraCenter         :: V3 a
                                 , _orthoCameraColatitude     :: a
                                 , _orthoCameraLongitude      :: a
                                 , _orthoCameraScale          :: a
                                 , _orthoCameraViewportWidth  :: Int
                                 , _orthoCameraViewportHeight :: Int
                                 , _orthoCameraZoomModel      :: ZoomModel a
                                 } deriving (Show, Eq)

makeLenses ''OrthoCamera

orthoCamera :: Num a => ZoomModel a -> Int -> Int -> OrthoCamera a
orthoCamera z@(ZoomModel zs) w h = OrthoCamera zero 0 0 (NE.last zs) w h z

-- transform matrices

orthoCameraMatrix :: (RealFloat a, Epsilon a) => OrthoCamera a -> M44 a
orthoCameraMatrix cam = scale cam !*! rotateColat cam !*! rotateLong cam !*! translate cam

invOrthoCameraMatrix :: (RealFloat a, Epsilon a) => OrthoCamera a -> M44 a
invOrthoCameraMatrix cam = invTranslate cam !*! invRotateLong cam !*! invRotateColat cam !*! invScale cam

-- TODO linear has an ortho matrix that does scale + transl, use it? appearently no

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


zoomOut :: (Ord a, Num a) => OrthoCamera a -> OrthoCamera a
zoomOut cam = cam & orthoCameraScale %~ zoomModelOut (cam ^. orthoCameraZoomModel)
  where
    zoomModelOut (ZoomModel zooms) z = NE.head $ foldr NE.cons (NE.last zooms :| []) (NE.dropWhile (<= z) zooms)

zoomIn :: (Ord a, Num a) => OrthoCamera a -> OrthoCamera a
zoomIn cam = cam & orthoCameraScale %~ zoomModelIn (cam ^. orthoCameraZoomModel)
  where zoomModelIn (ZoomModel zooms) z = NE.last $ NE.head zooms :| NE.takeWhile (< z) zooms

-- transformation parts

orthoCameraAspectRatio :: (RealFloat a) => OrthoCamera a -> a
orthoCameraAspectRatio cam =
    realToFrac (cam ^. orthoCameraViewportWidth) / realToFrac (cam ^. orthoCameraViewportHeight)


translate :: Num a => OrthoCamera a -> M44 a
translate = translate' . negate . view orthoCameraCenter

scale :: RealFloat a => OrthoCamera a -> M44 a
scale cam = scale' (cam ^. orthoCameraScale) (orthoCameraAspectRatio cam) (cam ^. orthoCameraZoomModel . maxZoom)

rotateLong :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
rotateLong = rotateZ . view orthoCameraLongitude

rotateColat :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
rotateColat = rotateX . view orthoCameraColatitude

-- inverse transformation parts

invTranslate :: Num a => OrthoCamera a -> M44 a
invTranslate = translate' . view orthoCameraCenter

invScale :: RealFloat a => OrthoCamera a -> M44 a
invScale cam = scale' (1 / cam ^. orthoCameraScale)
                      (1 / orthoCameraAspectRatio cam)
                      (1 / cam ^. orthoCameraZoomModel . maxZoom)

invRotateLong :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
invRotateLong = rotateZ . negate . view orthoCameraLongitude

invRotateColat :: (Epsilon a, Floating a) => OrthoCamera a -> M44 a
invRotateColat = rotateX . negate . view orthoCameraColatitude


-- primitive transformations

rotateZ :: (Epsilon a, Floating a) => a -> M44 a
rotateZ a = mkTransformation (axisAngle (V3 0 0 1) a) zero

rotateX :: (Epsilon a, Floating a) => a -> M44 a
rotateX a = mkTransformation (axisAngle (V3 1 0 0) a) zero

scale' :: (RealFloat a) => a -> a -> a -> M44 a
scale' sz rat maxScale =
    scaled $ V4 0 0 0 1 & _xy .~ (1 / sz) *^ ratioScale & _z .~ 1 / maxScale
  where
    ratioScale = if rat > 1 then V2 (1/rat) 1 else V2 1 rat

translate' :: (Num a) => V3 a -> M44 a
translate' v = identity & column _w . _xyz .~ v

