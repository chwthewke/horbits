{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Horbits.UI.ShowDim
    (ShowDim, showUnit, showQuantityWith, showQuantity, showQuantityShort, showQuantitySci, showQuantitySciShort,
    showNth, showBodyLevel, showDegreeAngle, showRadianAngle, showVelocity, showGravity,
    showKerbalTime, showKerbalTimeSplit, showOrbitalDistance, showPlanetaryDistance, showOrbitalVelocity)
  where

import           Control.Lens                hiding ((*~))
import           Numeric.NumType.TF          (NumType, toNum)
import           Prelude                     (abs, mod, (+))
import           Text.Printf.TH

import           Horbits.Body
import           Horbits.Dimensional.Prelude hiding (abs, mod, (+), _2)
import           Horbits.Orbit
import           Horbits.SolarSystem
import           Horbits.Time

-- Primitives

showUnit :: ShowDim d => Quantity d a -> (a, String)
showUnit q = (q ^. from dim, showDim q)

showQuantityWith :: ShowDim d => (a -> String -> String) -> Quantity d a -> String
showQuantityWith fmt = uncurry fmt . showUnit

showQuantity :: (RealFloat a, ShowDim d) => Quantity d a -> String
showQuantity = showQuantityWith [s|%f%s|]

showQuantityShort :: (RealFloat a, ShowDim d) => Quantity d a -> String
showQuantityShort = showQuantityWith [s|%.2f%s|]

-- TODO Regex, Attoparsec, whatever :/
translateExponent :: String -> String
translateExponent [] = []
translateExponent ('e':e) = "\xD7\&10<sup>" ++ translateExponentEnd e
translateExponent (c:cs) = c:translateExponent cs

translateExponentEnd :: String -> String
translateExponentEnd [] = "</sup>"
translateExponentEnd cs@(' ':_) = "</sup>" ++ cs
translateExponentEnd (c:cs) = c:translateExponentEnd cs

showQuantitySci :: (RealFloat a, ShowDim d) => Quantity d a -> String
showQuantitySci = translateExponent . showQuantityWith [s|%e%s|]

showQuantitySciShort :: (RealFloat a, ShowDim d) => Quantity d a -> String
showQuantitySciShort = translateExponent . showQuantityWith [s|%.3e%s|]

-- Shows for our quantities and other data types

showBodyLevel :: BodyLevel -> String
showBodyLevel (Star _) = "Star"
showBodyLevel (Planet i p _) = showNth i ++ " planet of " ++ p ^. fromBodyId . bodyName
showBodyLevel (Moon i p _) = showNth i ++ " moon of " ++ p ^. fromBodyId . bodyName

showNth :: (Integral a, Show a) => a -> String
showNth x | x `mod` 10 == 1 && x `mod` 100 /= 11 = [s|%dst|] x
          | x `mod` 10 == 2 && x `mod` 100 /= 12 = [s|%dnd|] x
          | x `mod` 10 == 3 && x `mod` 100 /= 13 = [s|%drd|] x
          | otherwise                            = [s|%dth|] x

showDegreeAngle :: Dimensionless Double -> String
showDegreeAngle = [s|%.1f\xB0|] . fst . showUnit . (/ (1 *~ degree))

showRadianAngle :: Dimensionless Double -> String
showRadianAngle = [s|%.2f rad|] . fst . showUnit

showVelocity :: Velocity Double -> String
showVelocity = showQuantityWith [s|%.1f%s|]

showGravity :: Acceleration Double -> String
showGravity g = showQuantityShort g ++
    " (" ++ showQuantityShort (g / Kerbin ^. fromBodyId . bodySurfaceGravity) ++ " g)"

_showKerbalTime :: Time Double -> (String, String)
_showKerbalTime t =
    ([s|%s %.2f s|]
        initText
        (fromIntegral (k ^. seconds) + k ^. secondsFraction)
    , [s|(%.2f %s)|]
        q
        u)
  where (q, u) = showUnit t
        k = t ^. from duration
        optionalParts = dropWhile ((== 0) . fst) $ zip (k ^. timeComponents . _2 . reversed) ["y", "d", "h", "m"]
        initText = unwords . fmap (uncurry [s|%d %s|]) $ optionalParts

showKerbalTime :: Time Double -> String
showKerbalTime t = t1 ++ " " ++ t2
  where (t1, t2) = _showKerbalTime t

showKerbalTimeSplit :: Time Double -> String
showKerbalTimeSplit t = t1 ++ "\n" ++ t2
  where (t1, t2) = _showKerbalTime t

showOrbitalDistance :: Length Double -> String
showOrbitalDistance = showQuantityWith [s|%.0f%s|]

showPlanetaryDistance :: Length Double -> String
showPlanetaryDistance = showQuantityWith [s|%.0f%s|]

showOrbitalVelocity :: OrbitalVelocity -> String
showOrbitalVelocity (OrbitalVelocity minV maxV) = [s|%.1f-%.1f%s|] q q' u
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
    showDim q = if null (renderDim q) then "" else " " ++ renderDim q

instance forall d a. ShowDim d => ShowDim (Quantity d a) where
    showDim _ = showDim (undefined :: d)

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

