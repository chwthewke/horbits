{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}

-- TODO move/rename Horbits.Numeric.Units.....
-- TODO make a prelude?
-- TODO single Linear import
module Horbits.Dimensional.Prelude(
    module Horbits.Dimensional.Prelude,
    module X)
  where

import           Control.Lens                         hiding ((*~), _1)
import           Data.Fixed                           (mod')
import qualified Linear                               as L
import           Numeric.NumType.TF                   (Pos2)
import           Numeric.Units.Dimensional.TF         (Dimensional (Dimensional))
import qualified Numeric.Units.Dimensional.TF         as D
import qualified Prelude                              as P

import           Horbits.Dimensional.Internal

import           Linear                               as X hiding (axisAngle, cross, distance, dot, nearZero, norm,
                                                            normalize, project, qd, quadrance, rotate, signorm, (*^),
                                                            (^*), (^+^), (^-^), (^/), _x, _xy, _xz, _y, _yx, _yz, _z,
                                                            _zx, _zy)
import           Numeric.Units.Dimensional.TF.Prelude as X hiding (atan2, mod, subtract, zero, (^/))
import           Prelude                              as X hiding (abs, acos, acosh, asin, asinh, atan, atan2, atanh,
                                                            cos, cosh, exp, log, mod, negate, pi, sin, sinh, sqrt,
                                                            subtract, sum, tan, tanh, (*), (**), (+), (-), (/), (^))

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/
infixl 7 *.

(*.) :: (Num a, d ~ Mul DOne d) => a -> Quantity d a -> Quantity d a
a *. q = (a *~ one) * q

-- lenses

-- TODO name dimensional?
dim :: Iso' a (Dimensional v d a)
dim = iso Dimensional undim
  where undim (Dimensional a) = a

_x :: (R1 f) => Lens' (Quantity d (f a)) (Quantity d a)
_x = from dim . L._x . dim

_y :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d a)
_y = from dim . L._y . dim

_xy :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_xy = from dim . L._xy . dim

_yx :: (R2 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_yx = from dim . L._yx . dim

_z :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d a)
_z = from dim . L._z . dim

_xz :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_xz = from dim . L._xz . dim

_zx :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_zx = from dim . L._zx . dim

_yz :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_yz = from dim . L._yz . dim

_zy :: (R3 f) => Lens' (Quantity d (f a)) (Quantity d (V2 a))
_zy = from dim . L._zy . dim


-- subtract

subtract :: Num a => Quantity d a -> Quantity d a -> Quantity d a
subtract = flip (-)

-- Real modulo

mod :: Real a => Quantity d a -> Quantity d a -> Quantity d a
mod = liftDA2 mod'

-- Trig

atan2 :: (RealFloat a, Div d d ~ DOne) =>
            Quantity d a -> Quantity d a -> Dimensionless a
atan2 = liftDD2 atan2'
  where atan2' y' (-0) = P.atan2 y' 0
        atan2' y' x' = P.atan2 y' x'

-- Vector

dimZero :: (Additive f, Num a) => Quantity d (f a)
dimZero = Dimensional L.zero

(^+^) :: (Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^+^) = liftDA2 (L.^+^)

(^-^) :: (Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^-^) = liftDA2 (L.^-^)

(^*) :: (Num a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (Mul d d') (f a)
(^*) = liftDM2 (L.^*)

(*^) :: (Num a, Functor f) => Quantity d a -> Quantity d' (f a) -> Quantity (Mul d d') (f a)
(*^) = liftDM2 (L.*^)

(^/) :: (Fractional a, Functor f) => Quantity d (f a) -> Quantity d' a -> Quantity (Div d d') (f a)
(^/) = liftDD2 (L.^/)

v2 :: Quantity d a -> Quantity d a -> Quantity d (V2 a)
v2 = liftDA2 V2

v3 :: Quantity d a -> Quantity d a -> Quantity d a -> Quantity d (V3 a)
v3 = liftDA3 V3

cross :: (Num a) => Quantity d (V3 a) -> Quantity d' (L.V3 a) -> Quantity (Mul d d') (V3 a)
cross = liftDM2 L.cross

-- Epsilon

nearZeroOf :: (Fractional a, Epsilon a) => Quantity d a -> Quantity d a -> Bool
nearZeroOf u q = dimensionlessNearZero $ q D./ u
  where dimensionlessNearZero (Dimensional x) = L.nearZero x

nearZero :: (Fractional a, Epsilon a) => Dimensionless a -> Bool
nearZero = nearZeroOf _1

-- Metric

dot :: (Metric f, Num a) => Quantity d (f a) -> Quantity d' (f a) -> Quantity (Mul d d') a
dot = liftDM2 L.dot

quadrance :: (Metric f, Num a) => Quantity d (f a) -> Quantity (Pow d Pos2) a
quadrance = liftDPow pos2 L.quadrance

qd :: (Metric f, Num a, Mul d d ~ Pow d Pos2) => Quantity d (f a) -> Quantity d (f a) -> Quantity (Pow d Pos2) a
qd = liftDM2 L.qd

distance :: (Metric f, Floating a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d a
distance = liftDA2 L.distance

norm :: (Metric f, Floating a) => Quantity d (f a) -> Quantity d a
norm = liftDLin L.norm

signorm :: (Metric f, Floating a) => Quantity d (f a) -> Dimensionless (f a)
signorm = liftDOne L.signorm

normalize :: (Metric f, Floating a, L.Epsilon a) => Quantity d (f a) -> Dimensionless (f a)
normalize = liftDOne L.normalize

project :: (Metric f, Fractional a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
project = liftDA2 L.project

-- Quaternion

type Rotation a = Dimensionless (Quaternion a)

rotate :: (Conjugate a, RealFloat a) =>
          Rotation a -> Quantity d (V3 a) -> Quantity d (V3 a)
rotate (Dimensional q) = liftDLin $ L.rotate q

axisAngle :: (Floating a, Epsilon a) => Quantity d (V3 a) -> Dimensionless a -> Rotation a
axisAngle = liftDOne2 L.axisAngle

rotX :: (Floating a, Epsilon a) => Dimensionless a -> Rotation a
rotX = axisAngle $ v3 _1 _0 _0

rotZ :: (Floating a, Epsilon a) => Dimensionless a -> Rotation a
rotZ = axisAngle $ v3 _0 _0 _1
