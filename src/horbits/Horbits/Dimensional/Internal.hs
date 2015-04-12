module Horbits.Dimensional.Internal (DF1, DF2, DF3,
    liftDA2, liftDA3, liftDD2, liftDOne, liftDOne2, liftDLin, liftDM2, liftDPow)
  where

import           Numeric.Units.Dimensional.TF

-- Applicative-like lifts
-- TODO replace with a wrapped applicative instance?

type DF1 v d d' a b = (a -> b) -> Dimensional v d a -> Dimensional v d' b

liftD :: DF1 v d d' a b
liftD f (Dimensional a) = Dimensional $ f a

liftDOne :: DF1 v d DOne a b
liftDOne = liftD

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

liftDOne2 :: DF2 v d d' DOne a b c
liftDOne2 = liftD2

liftDM2 :: DF2 v d d' (Mul d d') a b c
liftDM2 = liftD2

liftDD2 :: DF2 v d d' (Div d d') a b c
liftDD2 = liftD2

type DF3 v d d' d'' d''' a b c e = (a -> b -> c -> e)
            -> Dimensional v d a
            -> Dimensional v d' b
            -> Dimensional v d'' c
            -> Dimensional v d''' e

liftDA3 :: DF3 v d d d d a b c e
liftDA3 f (Dimensional a) (Dimensional b) (Dimensional c) = Dimensional $ f a b c
