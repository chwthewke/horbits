{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeFamilies          #-}

module Horbits.OrbitEq
                       ((&&&),
                        (|||),
                        AbsoluteApproximateEq(..),
                        RelativeApproximateEq(..),
                        closeTo,
                        relativelyCloseTo,
                        relativelyClose)
  where

import           Control.Applicative
import           Control.Lens                         hiding (_1)
import           Control.Rematch
import           Horbits.DimLin
import           Horbits.Orbit
import           Linear.Metric                        (Metric)
import           Numeric.Units.Dimensional.TF.Prelude hiding (mod)
import           Prelude                              hiding (abs, mod, pi, (*), (+), (-))

infix 4 =~, =~~
infixr 3 &&&, |||

(&&&) :: Applicative f => f Bool -> f Bool -> f Bool
(&&&) = liftA2 (&&)

(|||) :: Applicative f => f Bool -> f Bool -> f Bool
(|||) = liftA2 (||)

class AbsoluteApproximateEq a b where
    (=~) :: a -> a -> b -> Bool
    absClose :: a -> a -> b -> Bool
    absClose = (=~)

class RelativeApproximateEq a b where
    (=~~) :: a -> a -> b -> Bool
    relClose :: a -> a -> b -> Bool
    relClose = (=~~)

instance AbsoluteApproximateEq (Quantity d Double) (Quantity d Double) where
    (=~) actual expected tolerance = abs (expected - actual) <= abs tolerance

instance (d ~ Mul DOne d) => RelativeApproximateEq (Quantity d Double) (Dimensionless Double) where
    (=~~) actual expected tolerance = abs (expected - actual) <= tolerance * abs expected

instance (d ~ Mul DOne d, Metric f) => RelativeApproximateEq (Quantity d (f Double)) (Dimensionless Double) where
    (=~~) actual expected tolerance = norm (actual ^-^ expected) <= tolerance * norm expected

instance RelativeApproximateEq Orbit (Dimensionless Double) where
    (=~~) actual expected =
        (actual ^. orbitEccentricityVector) =~~ (expected ^. orbitEccentricityVector) &&&
            (actual ^. orbitAngularMomentumVector) =~~ (expected ^. orbitAngularMomentumVector)

approxMatch :: (Show a, Show t) => (a -> a -> t -> Bool) -> a -> t -> Matcher a
approxMatch appr expected tolerance = Matcher (\actual -> appr actual expected tolerance)
                                              ("within " ++ show tolerance ++ " of " ++ show expected)
                                              standardMismatch

approxMatchSym :: (Show a, Show t) => (a -> a -> t -> Bool) -> t -> Matcher (a, a)
approxMatchSym appr tolerance = Matcher (\(l, r) -> appr l r tolerance)
                                        ("within " ++ show tolerance ++ " of each other")
                                        standardMismatch

closeTo :: (Show a, Show t, AbsoluteApproximateEq a t) => a -> t -> Matcher a
closeTo = approxMatch (=~)

relativelyCloseTo :: (Show a, Show t, RelativeApproximateEq a t) => a -> t -> Matcher a
relativelyCloseTo = approxMatch (=~~)

relativelyClose :: (Show a, Show t, RelativeApproximateEq a t) => t -> Matcher (a, a)
relativelyClose = approxMatchSym (=~~)


