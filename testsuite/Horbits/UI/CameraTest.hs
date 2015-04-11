{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Horbits.UI.CameraTest where

import           Control.Applicative
import           Data.Foldable              (Foldable, all)
import           Data.Traversable           (Traversable)
import           Linear
import           Prelude                    hiding (all)
import           Test.Framework             hiding (scale)

import           Horbits.UI.Camera
import           Horbits.UI.Camera.Internal

genCamera :: Gen (OrthoCamera Double)
genCamera = do
    cx <- double
    cy <- double
    cz <- double
    col <- choose (0, pi)
    lng <- choose (0, 2 * pi)
    s <- double
    w <- int
    h <- int
    return $ OrthoCamera (V3 cx cy cz) col lng s w h $ linearZoom 1 (1, 10)
  where
    double = choose (0, 1000) `suchThat` (> 0)
    int = choose (100, 2000)

nearZeroMatrix :: (Epsilon a, Foldable m, Foldable n) => m (n a) -> Bool
nearZeroMatrix = all (all nearZero)

nearIdentityMatrix :: (Epsilon a, Traversable m, Applicative m, Additive m) => m (m a) -> Bool
nearIdentityMatrix = nearZeroMatrix . (!-! identity)

prop_translateIsInvertible :: Property
prop_translateIsInvertible =
    forAll genCamera $ \c ->
        nearIdentityMatrix $ translate c !*! invTranslate c

prop_rotateLongIsInvertible :: Property
prop_rotateLongIsInvertible =
    forAll genCamera $ \c ->
        nearIdentityMatrix $ rotateLong c !*! invRotateLong c

prop_rotateColatIsInvertible :: Property
prop_rotateColatIsInvertible =
    forAll genCamera $ \c ->
        nearIdentityMatrix $ rotateColat c !*! invRotateColat c

prop_scaleIsInvertible :: Property
prop_scaleIsInvertible =
    forAll genCamera $ \c ->
        nearIdentityMatrix $ scale c !*! invScale c

prop_cameraMatrixIsInvertible :: Property
prop_cameraMatrixIsInvertible =
    forAll genCamera $ \c ->
        nearIdentityMatrix $ orthoCameraMatrix c !*! invOrthoCameraMatrix c
