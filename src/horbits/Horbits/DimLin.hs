{-# LANGUAGE TypeFamilies #-}

module Horbits.DimLin(Horbits.DimLin.atan2, _x, _y, _z, _xy, _yx, zero, (^+^), (^-^), (^*), (*^), (^/), (*.),
    cross, dot, dimNearZero, quadrance, qd, distance, Horbits.DimLin.mod, norm, signorm, normalize, project,
    rotate, rotX, rotZ, v2, v3, V1, V2, V3) where

import           Control.Lens                 hiding ((*~))
import qualified Data.Fixed                   as DF
import           Horbits.Types                ()
import           Linear.Conjugate             (Conjugate)
import qualified Linear.Epsilon               as E
import qualified Linear.Metric                as N
import qualified Linear.Quaternion            as Q
import           Linear.V1                    (R1, V1)
import qualified Linear.V1                    as V1 (_x)
import           Linear.V2                    (R2, V2 (..))
import qualified Linear.V2                    as V2 (_xy, _y, _yx)
import           Linear.V3                    (R3, V3 (..))
import qualified Linear.V3                    as V3 (cross, _z)
import qualified Linear.Vector                as V
import           Numeric.NumType.TF           (Pos2, pos2)
import           Numeric.Units.Dimensional.TF (DOne, Dimensional (..), Dimensionless, Div, Mul, Pow, Quantity, one, (*),
                                               (*~))
import qualified Numeric.Units.Dimensional.TF as D
import           Prelude                      hiding ((*))

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/
infixl 7 *.

(*.) :: (Num a, d ~ Mul DOne d) => a -> Quantity d a -> Quantity d a
a *. q = (a *~ one) * q

-- lenses

dim :: Iso' a (Dimensional v d a)
dim = iso Dimensional undim
  where undim (Dimensional a) = a

_x :: (R1 f) => Lens' (Quantity d (f a)) (Quantity d a)
_x = from dim . V1._x . dim

_y :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d a)
_y = from dim . V2._y . dim

_xy :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_xy = from dim . V2._xy . dim

_yx :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_yx = from dim . V2._yx . dim

_z :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d a)
_z = from dim . V3._z . dim

-- Applicative-like lifts

type DF1 v d d' a b = (a -> b) -> Dimensional v d a -> Dimensional v d' b

liftD :: DF1 v d d' a b
liftD f (Dimensional a) = Dimensional $ f a

liftDLin :: DF1 v d d a b
liftDLin = liftD

liftDPow :: n -> DF1 v d (Pow d n) a b
liftDPow _ = liftD

type DF2 v d d' d'' a b c = (a -> b -> c) -> Dimensional v d a -> Dimensional v d' b -> Dimensional v d'' c

liftD2 :: DF2 v d d' d'' a b c
liftD2 f (Dimensional a) (Dimensional b) = Dimensional $ f a b

-- those are just reminders that enforce a relation between in/out dimensions, so, useful?
liftDA2 :: DF2 v d d d a b c
liftDA2 = liftD2

liftDM2 :: DF2 v d d' (Mul d d') a b c
liftDM2 = liftD2

liftDD2 :: DF2 v d d' (Div d d') a b c
liftDD2 = liftD2

-- Real modulo

mod :: (Real a) => Quantity d a -> Quantity d a -> Quantity d a
mod = liftDA2 DF.mod'

-- Trig

atan2 :: RealFloat a =>
            Quantity d a -> Quantity d a -> Dimensionless a
atan2 (Dimensional y) (Dimensional x) = Dimensional $ atan2' y x
  where atan2' y' (-0) = Prelude.atan2 y' 0
        atan2' y' x' = Prelude.atan2 y' x'

-- Vector

zero :: (V.Additive f, Num a) => Quantity d (f a)
zero = Dimensional V.zero

(^+^) :: (V.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^+^) = liftDA2 (V.^+^)

(^-^) :: (V.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^-^) = liftDA2 (V.^-^)

(^*) :: (Num a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (Mul d d') (f a)
(^*) = liftDM2 (V.^*)

(*^) :: (Num a, Functor f) => Quantity d a -> Quantity d' (f a) -> Quantity (Mul d d') (f a)
(*^) = liftDM2 (V.*^)

(^/) :: (Fractional a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (Div d d') (f a)
(^/) = liftDD2 (V.^/)

v2 :: Quantity d a -> Quantity d a -> Quantity d (V2 a)
v2 (Dimensional x) (Dimensional y) = Dimensional $ V2 x y

v3 :: Quantity d a -> Quantity d a -> Quantity d a -> Quantity d (V3 a)
v3 (Dimensional x) (Dimensional y) (Dimensional z) = Dimensional $ V3 x y z

cross :: (Num a) => Quantity d (V3 a) -> Quantity d' (V3 a) -> Quantity (Mul d d') (V3 a)
cross = liftDM2 V3.cross

-- Epsilon

dimNearZero :: (Fractional a, E.Epsilon a) => Quantity d a -> Quantity d a -> Bool
dimNearZero u q = dimensionlessNearZero $ q D./ u
  where dimensionlessNearZero (Dimensional x) = E.nearZero x

-- Metric

dot :: (N.Metric f, Num a) => Quantity d (f a) -> Quantity d' (f a) -> Quantity (Mul d d') a
dot = liftDM2 N.dot

quadrance :: (N.Metric f, Num a) => Quantity d (f a) -> Quantity (Pow d Pos2) a
quadrance = liftDPow pos2 N.quadrance

qd :: (N.Metric f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity (Pow d Pos2) a
qd = liftD2 N.qd

distance :: (N.Metric f, Floating a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d a
distance = liftDA2 N.distance

norm :: (N.Metric f, Floating a) => Quantity d (f a) -> Quantity d a
norm = liftDLin N.norm

signorm :: (N.Metric f, Floating a) => Quantity d (f a) -> Dimensionless (f a)
signorm = liftD N.signorm

normalize :: (N.Metric f, Floating a, E.Epsilon a) => Quantity d (f a) -> Dimensionless (f a)
normalize = liftD N.normalize

project :: (N.Metric f, Fractional a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
project = liftDA2 N.project

-- Quaternion

rotate :: (Linear.Conjugate.Conjugate a, RealFloat a) =>
          Dimensionless (Q.Quaternion a) -> Quantity d (V3 a) -> Quantity d (V3 a)
rotate (Dimensional q) = liftDLin $ Q.rotate q

rotX :: Dimensionless Double -> Dimensionless (Q.Quaternion Double)
rotX = fmap rotX'
--  where rotX' theta = Q.Quaternion (cos $ theta / 2) (V3 (sin $ theta / 2) 0 0)
  where rotX' = Q.axisAngle (V3 1 0 0)


rotZ :: Dimensionless Double -> Dimensionless (Q.Quaternion Double)
rotZ = fmap rotZ'
--  where rotZ' theta = Q.Quaternion (cos $ theta / 2) (V3 0 0 (sin $ theta / 2))
  where rotZ' = Q.axisAngle (V3 0 0 1)
