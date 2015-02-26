{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Horbits.UI.ShowDim
    (ShowDim, showUnit, showQuantityWith, showQuantity, showQuantitySci,
    showNth, showBodyPosition, showDegreeAngle, showRadianAngle,
    showKerbalTime, showOrbitalDistance, showPlanetaryDistance, showOrbitalVelocity)
  where

import           Control.Lens                         hiding ((*~))
import           Horbits.Body
import           Horbits.KerbalDateTime
import           Horbits.Orbit
import           Horbits.SolarSystem
import           Numeric.NumType.TF                   (NumType, toNum)
import           Numeric.Units.Dimensional.TF         (Dimensional (Dimensional))
import           Numeric.Units.Dimensional.TF.Prelude hiding (abs, (+))
import           Prelude                              hiding ((/))
import           Text.Printf.TH

-- Primitives

showUnit :: ShowDim d => Quantity d a -> (a, String)
showUnit q@(Dimensional x) = (x, showDim q)

showQuantityWith :: ShowDim d => (a -> String -> String) -> Quantity d a -> String
showQuantityWith fmt = uncurry fmt . showUnit

showQuantity :: (RealFloat a, ShowDim d) => Quantity d a -> String
showQuantity = showQuantityWith [s|%f %s|]

-- TODO "e(\d+)" ->  "x10<sup>\1</sup>"
showQuantitySci :: (RealFloat a, ShowDim d) => Quantity d a -> String
showQuantitySci = showQuantityWith [s|%e %s|]

-- Shows for our quantities and other data types

showBodyPosition :: BodyPosition -> String
showBodyPosition (Sun _) = "Sun"
showBodyPosition (Planet i p _) = showNth i ++ " planet of " ++ p ^. fromBodyId . bodyName
showBodyPosition (Moon i p _) = showNth i ++ " moon of " ++ p ^. fromBodyId . bodyName

showNth :: (Integral a, Show a) => a -> String
showNth x | x `mod` 10 == 1 && x `mod` 100 /= 11 = [s|%dst|] x
          | x `mod` 10 == 2 && x `mod` 100 /= 12 = [s|%dnd|] x
          | x `mod` 10 == 3 && x `mod` 100 /= 13 = [s|%drd|] x
          | otherwise                            = [s|%dth|] x

showDegreeAngle :: Dimensionless Double -> String
showDegreeAngle = [s|%.1f\xB0|] . fst . showUnit . (/ (1 *~ degree))

showRadianAngle :: Dimensionless Double -> String
showRadianAngle = [s|%.1f rad|] . fst . showUnit

-- TODO hide leading zeroes
showKerbalTime :: Time Double -> String
showKerbalTime t =
    [s|%d y %d d %d h %d m %.2f s (%f %s)|]
        (k ^. years)
        (k ^. days)
        (k ^. hours)
        (k ^. minutes)
        (fromIntegral (k ^. seconds) + k ^. secondsFraction)
        q
        u
  where (q, u) = showUnit t
        k = t ^. from duration

showOrbitalDistance :: Length Double -> String
showOrbitalDistance = showQuantityWith [s|%.0f %s|]

showPlanetaryDistance :: Length Double -> String
showPlanetaryDistance = showQuantityWith [s|%.0f %s|]

showOrbitalVelocity :: OrbitalVelocity -> String
showOrbitalVelocity (OrbitalVelocity minV maxV) = [s|%f-%f %s|] q q' u
  where
    (q, u) = showUnit minV
    (q', _) = showUnit maxV

-- Underlying dimensional machinery

class ShowDim a where
    showDim :: a -> String

instance ( NumType l
         , NumType m
         , NumType t
         , NumType i
         , NumType th
         , NumType n
         , NumType j ) =>
             ShowDim (Dim l m t i th n j) where
    showDim = renderDim

instance forall d a. ShowDim d => ShowDim (Quantity d a) where
    showDim (Dimensional _) = showDim (undefined :: d)

data Sign a = Null | Pos a | Neg a

pos :: Sign a -> [a]
pos (Pos x) = [x]
pos _ = []

neg :: Sign a -> [a]
neg (Neg x) = [x]
neg _ = []


renderDim1 :: NumType n => String -> n -> Sign String
renderDim1 u n
    | x == 0 = Null
    | x == 1 = Pos u
    | x == -1 = Neg u
    | x > 0 = Pos $ sup x
    | otherwise = Neg $ sup (-x)
  where
    x = toNum n :: Integer
    sup p = u ++ "<sup>" ++ show (abs p) ++ "</sup>"

renderDim :: forall l m t i th n j.
             ( NumType l
             , NumType m
             , NumType t
             , NumType i
             , NumType th
             , NumType n
             , NumType j ) =>
             Dim l m t i th n j -> String
renderDim _
    | null nDims = pDims
    | otherwise = pDims ++ "/" ++ nDims
  where
    dims = [ renderDim1 "m" (undefined :: l)
           , renderDim1 "kg" (undefined :: m)
           , renderDim1 "s" (undefined :: t)
           , renderDim1 "A" (undefined :: i)
           , renderDim1 "K" (undefined :: th)
           , renderDim1 "mol" (undefined :: n)
           , renderDim1 "cd" (undefined :: j) ]
    pDims = unwords $ concatMap pos dims
    nDims = unwords $ concatMap neg dims

