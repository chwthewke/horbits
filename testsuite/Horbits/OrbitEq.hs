{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import           Control.Rematch
import           Numeric.Units.Dimensional.TF.Prelude hiding (mod)
import           Prelude                              hiding (abs, mod, pi, (*),
                                                       (+), (-))

-- TODO TypeFamilies or FunDeps might help with the ascriptions in isEquatorial, isCircular,
-- but wait until we have all instances

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



approxMatch :: (Show a, Show t) =>
                 (a -> a -> t -> Bool) -> a -> t -> Matcher a
approxMatch op expected tolerance = Matcher (\actual -> op actual expected tolerance)
                                            ("within " ++ show tolerance ++ " of " ++ show expected)
                                            standardMismatch

approxMatchSym :: (Show a, Show t) =>
                 (a -> a -> t -> Bool) -> t -> Matcher (a, a)
approxMatchSym op tolerance = Matcher (\(l, r) -> op l r tolerance)
                                      ("within " ++ show tolerance ++ " of each other")
                                      standardMismatch

closeTo :: (Show a, Show t, AbsoluteApproximateEq a t) => a -> t -> Matcher a
closeTo = approxMatch (=~)

relativelyCloseTo :: (Show a, Show t, RelativeApproximateEq a t) => a -> t -> Matcher a
relativelyCloseTo = approxMatch (=~~)

relativelyClose :: (Show a, Show t, RelativeApproximateEq a t) => t -> Matcher (a, a)
relativelyClose = approxMatchSym (=~~)


