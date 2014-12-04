{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE UndecidableInstances  #-}

module Horbits.OrbitEq
                       ((&&&),
                        (|||),
                        AbsoluteApproximateEq(..),
                        RelativeApproximateEq(..),
                        classicalElementsApproximatelyEqualExpected,
                        sampleClassicalElementsApproximatelyEqualExpected)
  where

import           Control.Applicative
import           Control.Rematch
import           Horbits.DimLin
import           Horbits.Orbit
import           Horbits.OrbitSample
import           Horbits.Rematch
import           Numeric.Units.Dimensional.Prelude hiding (mod)
import           Prelude                           hiding (abs, mod, pi, (*),
                                                    (+), (-))
import           Test.QuickCheck                   hiding (sample)

-- TODO TypeFamilies might help with the ascriptions in isEquatorial, isCircular, but wait until we have all instances

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

instance (Mul DOne d d) => RelativeApproximateEq (Quantity d Double) (Dimensionless Double) where
  (=~~) actual expected tolerance = abs (expected - actual) <= tolerance * abs expected

isEquatorial :: OrbitSample -> Dimensionless Double -> Bool
isEquatorial OrbitSample{..} = incl `mod` pi =~ (_0 :: Dimensionless Double)

isCircular :: OrbitSample -> Dimensionless Double -> Bool
isCircular OrbitSample{..} = e =~ (_0 :: Dimensionless Double)



approxMatch :: (Show a, Show t) =>
                 (a -> a -> t -> Bool) -> a -> t -> Matcher a
approxMatch op expected tolerance = Matcher (\actual -> op actual expected tolerance)
                                            ("within " ++ show tolerance ++ " of " ++ show expected)
                                            standardMismatch

closeTo :: (Show a, Show t, AbsoluteApproximateEq a t) => a -> t -> Matcher a
closeTo = approxMatch (=~)

relativelyCloseTo :: (Show a, Show t, RelativeApproximateEq a t) => a -> t -> Matcher a
relativelyCloseTo = approxMatch (=~~)

matchRaan :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchRaan sample@OrbitSample{..} = do
  eq <- isEquatorial sample
  let expectedRaan = if eq then _0 else raan
  has ("RAAN", _rightAscensionOfAscendingNode) <$> closeTo expectedRaan

matchArgPe :: OrbitSample -> Dimensionless Double -> (String, Matcher Orbit)
matchArgPe sample@OrbitSample{..} = do
  circ <- isCircular sample
  let expectedArgPe = if circ then _0 else arg
  has ("ARGPE", _argumentOfPeriapsis) <$> closeTo expectedArgPe

matchClassicalElements :: OrbitSample -> Dimensionless Double -> Matcher Orbit
matchClassicalElements sample@OrbitSample{..} = allOf' <$> sequence
    [has ("SMA", _semiMajorAxis) <$> relativelyCloseTo sma,
     has ("ECC", _eccentricity) <$> closeTo e,
     has ("INC", _inclination) <$> closeTo incl,
     matchRaan sample,
     matchArgPe sample
    ]

classicalElementsApproximatelyEqualExpected :: Double -> OrbitSample -> Bool
classicalElementsApproximatelyEqualExpected tolerance sample =
  match (matchClassicalElements sample (tolerance *~ one)) (orbit sample)

sampleClassicalElementsApproximatelyEqualExpected :: Double -> OrbitSample -> Property
sampleClassicalElementsApproximatelyEqualExpected tolerance sample =
  matcherProperty (matchClassicalElements sample $ tolerance *~ one) (orbit sample)


