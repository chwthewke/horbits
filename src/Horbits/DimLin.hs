{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module Horbits.DimLin where

import           Linear                    (Epsilon)
import qualified Linear.Metric             as M
import qualified Linear.Vector             as V
import           Numeric.NumType           (Pos2, pos2)
import           Numeric.Units.Dimensional (Dimensional (..), Dimensionless,
                                            Div, Mul, Pow, Quantity)
import           Prelude                   hiding ((*), (/))

infixl 6 ^+^, ^-^
infixl 7 ^*, *^, ^/

-- Lifts

type DF1 v d d' a b = (a -> b) -> Dimensional v d a -> Dimensional v d' b

liftD :: DF1 v d d' a b
liftD f (Dimensional a) = Dimensional $ f a

liftDLin :: DF1 v d d a b
liftDLin = liftD

liftDPow :: (Pow d n d') => n -> DF1 v d d' a b
liftDPow _ = liftD

type DF2 v d d' d'' a b c = (a -> b -> c) -> Dimensional v d a -> Dimensional v d' b -> Dimensional v d'' c

liftD2 :: DF2 v d d' d'' a b c
liftD2 f (Dimensional a) (Dimensional b) = Dimensional $ f a b

-- those are just reminders that enforce a relation between in/out dimensions, so, useful?
liftDA2 :: DF2 v d d d a b c
liftDA2 = liftD2

liftDM2 :: (Mul d d' d'') => DF2 v d d' d'' a b c
liftDM2 = liftD2

liftDD2 :: (Div d d' d'') => DF2 v d d' d'' a b c
liftDD2 = liftD2

-- Vector

zero :: (V.Additive f, Num a) => Dimensional v d (f a)
zero = Dimensional V.zero

(^+^) :: (V.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^+^) = liftDA2 (V.^+^)

(^-^) :: (V.Additive f, Num a) => Quantity d (f a) -> Quantity d (f a) -> Quantity d (f a)
(^-^) = liftDA2 (V.^-^)

(^*) :: (Num a, Functor f, Mul d d' d'') => Quantity d (f a) -> Quantity d' a -> Quantity d'' (f a)
(^*) = liftDM2 (V.^*)

(*^) :: (Num a, Functor f, Mul d d' d'') => Quantity d a -> Quantity d' (f a) -> Quantity d'' (f a)
(*^) = liftDM2 (V.*^)

(^/) :: (Fractional a, Functor f, Div d d' d'') => Quantity d (f a) -> Quantity d' a -> Quantity d'' (f a)
(^/) = liftDD2 (V.^/)

-- Metric

dot :: (M.Metric f, Num a, Mul d d' d'') => Dimensional v d (f a) -> Dimensional v d' (f a) -> Dimensional v d'' a
dot = liftDM2 M.dot

quadrance :: (M.Metric f, Num a, Pow d Pos2 d') => Dimensional v d (f a) -> Dimensional v d' a
quadrance = liftDPow pos2 M.quadrance

qd :: (M.Metric f, Num a, Pow d Pos2 d') => Dimensional v d (f a) -> Dimensional v d (f a) -> Dimensional v d' a
qd = liftD2 M.qd

distance :: (M.Metric f, Floating a) => Dimensional v d (f a) -> Dimensional v d (f a) -> Dimensional v d a
distance = liftDA2 M.distance

norm :: (M.Metric f, Floating a) => Dimensional v d (f a) -> Dimensional v d a
norm = liftDLin M.norm

signorm :: (M.Metric f, Floating a) => Dimensional v d (f a) -> Dimensional v d (f a)
signorm = liftDLin M.signorm

normalize :: (M.Metric f, Floating a, Epsilon a) => Dimensionless (f a) -> Dimensionless (f a)
normalize = fmap M.normalize

project :: (M.Metric f, Fractional a) => Dimensional v d (f a) -> Dimensional v d (f a) -> Dimensional v d (f a)
project = liftDA2 M.project
