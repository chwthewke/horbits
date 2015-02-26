{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Horbits.KerbalDateTime
    (KerbalTimeComponents, KerbalTime'(), KerbalTime, KerbalInstant'(), KerbalInstant,
     secondsFraction, seconds, minutes, hours, days, years, duration, epoch)
  where

import           Control.Applicative
import           Control.Lens                         hiding ((*~))
import           Data.List                            (unfoldr)
import           Linear.Affine
import           Linear.Vector
import           Numeric.Units.Dimensional.TF         (Dimensional (Dimensional))
import           Numeric.Units.Dimensional.TF.Prelude (Time)

newtype KerbalTime' a = KerbalTime' { fractionalSeconds :: Identity a } deriving (Show, Eq, Ord, Functor, Applicative)

type KerbalTime = KerbalTime' Double

type KerbalInstant' = Point KerbalTime'

type KerbalInstant = KerbalInstant' Double

class KerbalTimeComponents t where
    timeComponents :: RealFrac a => Iso' (t a) (a, [Integer])

    secondsFraction :: RealFrac a => Lens' (t a) a
    secondsFraction = timeComponents . _1

    timeComponentLens :: RealFrac a => Int -> Lens' (t a) Integer
    timeComponentLens i = timeComponents . _2 . singular (element i)

    seconds :: RealFrac a => Lens' (t a) Integer
    seconds = timeComponentLens  0

    minutes :: RealFrac a => Lens' (t a) Integer
    minutes = timeComponentLens 1

    hours :: RealFrac a => Lens' (t a) Integer
    hours = timeComponentLens 2

    days :: RealFrac a => Lens' (t a) Integer
    days = timeComponentLens 3

    years :: RealFrac a => Lens' (t a) Integer
    years = timeComponentLens 4

instance KerbalTimeComponents KerbalTime' where
    timeComponents = isoNum . iso toParts fromParts

instance KerbalTimeComponents KerbalInstant' where
    timeComponents = iso (.-. origin) (origin .+^) . timeComponents

epoch :: KerbalInstant
epoch = origin & years .~ 1 & days .~ 1

duration :: Iso' KerbalTime (Time Double)
duration = isoNum . iso Dimensional (\(Dimensional s) -> s)

temporalSubdivisions :: Integral a => [a]
temporalSubdivisions = [ 60, 60, 6, 426 ]

isoNum :: Iso' (KerbalTime' a) a
isoNum = iso (runIdentity . fractionalSeconds) (KerbalTime' . Identity)

instance Additive KerbalTime' where
    zero = KerbalTime' $ Identity 0

type DecompState a = Maybe (a, [a])

decompose :: Integral a => DecompState a -> Maybe (a, DecompState a)
decompose Nothing = Nothing
decompose (Just (n, [])) = Just (n, Nothing)
decompose (Just (n, d:ds)) = Just (n `mod` d, Just (n `div` d, ds))

smhdy :: Integral a => a -> [a]
smhdy n = unfoldr decompose $ Just (n, temporalSubdivisions)

fromSmhdy :: Integral a => [a] -> a
fromSmhdy ips = foldr (\(c, n) x -> c * (x + n)) 0 $ zip (1 : temporalSubdivisions) ips

toParts :: RealFrac a => a -> (a, [Integer])
toParts x = (f, smhdy i)
  where (i, f) = properFraction x

fromParts :: Num a => (a, [Integer]) -> a
fromParts (f, ips) = f + fromIntegral (fromSmhdy ips)


