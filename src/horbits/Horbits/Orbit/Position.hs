{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}

module Horbits.Orbit.Position
    (OrbitPosition, bodyPosition,
    positionOrbit, positionDateTime, positionMeanAnomaly, positionEccentricAnomaly, positionTrueAnomaly,
    positionVector,
    eccentricToMeanAnomaly, meanToEccentricAnomaly,
    eccentricToTrueAnomaly, trueToEccentricAnomaly,
    instantToMeanAnomaly, trueAnomalyToPositionVector,
    orbitPositionFromInstant, orbitPositionFromMeanAnomaly,
    orbitPositionFromEccentricAnomaly, orbitPositionFromTrueAnomaly)
  where

import           Control.Applicative
import           Control.Lens                hiding ((*~), _1, _2)
import           Control.Monad
import           Data.List.NonEmpty          as NE

import           Horbits.Body
import           Horbits.Dimensional.Prelude
import           Horbits.Orbit.Class
import           Horbits.Orbit.Data
import           Horbits.Orbit.Geometry
import           Horbits.Orbit.Properties
import           Horbits.Time

data OrbitPosition t = OrbitPosition { _positionOrbit           :: t
                                     , positionDateTime         :: KerbalInstant
                                     , positionMeanAnomaly      :: Dimensionless Double
                                     , positionEccentricAnomaly :: Dimensionless Double
                                     , positionTrueAnomaly      :: Dimensionless Double
                                     , positionVector           :: Length (V3 Double)
                                     } deriving (Show, Eq)

makeLensesFor [("_positionOrbit", "positionOrbit")] ''OrbitPosition

eccentricToMeanAnomaly :: OrbitClass t => t -> Dimensionless Double -> Dimensionless Double
eccentricToMeanAnomaly orbit ea = ea - orbit ^. orbitEccentricity * sin ea

meanToEccentricAnomaly :: OrbitClass t => t -> Dimensionless Double -> Dimensionless Double
meanToEccentricAnomaly orbit ma = NE.last $ NE.unfold next ea0
      where
        e = orbit ^. orbitEccentricity
        dnext ean = (ma + e * sin ean - ean) / (_1 - e * cos ean)
        next ean = (ean, (ean +) <$> mfilter (not . nearZeroOf (0.001 *~ one)) (Just $ dnext ean))
        ea0 = if e > (0.8 *~ one) then pi else ma

eccentricToTrueAnomaly :: OrbitClass t => t -> Dimensionless Double -> Dimensionless Double
eccentricToTrueAnomaly orbit = _trueToFromEccentric (orbit ^. orbitEccentricity)

trueToEccentricAnomaly :: OrbitClass t => t -> Dimensionless Double -> Dimensionless Double
trueToEccentricAnomaly orbit = _trueToFromEccentric (negate $ orbit ^. orbitEccentricity)

_trueToFromEccentric :: Dimensionless Double -> Dimensionless Double -> Dimensionless Double
_trueToFromEccentric e a = atan2 (sqrt (_1 + e) * sin (a / _2)) (sqrt (_1 - e) * cos (a / _2))

instantToMeanAnomaly :: OrbitClass t => t -> KerbalInstant -> Dimensionless Double
instantToMeanAnomaly orbit instant = ma0 + (instant ^. isoInstant / p) `mod` tau
  where
    ma0 = orbit ^. orbitMeanAnomalyAtEpoch
    p = orbit ^. orbitPeriod

trueAnomalyToPositionVector :: OrbitClass t => t -> Dimensionless Double -> Length (V3 Double)
trueAnomalyToPositionVector = flip $ \ta -> do
    ev <- view orbitEccentricityVector
    let e = norm ev
    hv <- view orbitAngularMomentumVector
    mu <- view orbitMu
    let r = quadrance hv / (mu * (_1 + e * cos ta))
    (ux, uy) <- orbitSemiAxes
    return $ r *^ (cos ta *^ ux ^+^ sin ta *^ uy)


instantFromMeanAnomalyAndRevs :: OrbitClass t => t -> Dimensionless Double -> Integer -> KerbalInstant
instantFromMeanAnomalyAndRevs orbit ma n =
    (((fromIntegral n *. tau) + ma) * orbit ^. orbitPeriod) ^. from isoInstant

orbitPositionFromInstant :: OrbitClass t => t -> KerbalInstant -> OrbitPosition t
orbitPositionFromInstant orbit instant =
    OrbitPosition orbit instant ma ea ta pv
  where
    ma = instantToMeanAnomaly orbit instant
    ea = meanToEccentricAnomaly orbit ma
    ta = eccentricToTrueAnomaly orbit ea
    pv = trueAnomalyToPositionVector orbit ta


orbitPositionFromMeanAnomaly :: OrbitClass t => t -- ^ orbit
                                             -> Dimensionless Double -- ^ mean anomaly
                                             -> Integer -- ^ full revolutions since epoch
                                             -> OrbitPosition t
orbitPositionFromMeanAnomaly orbit ma n = OrbitPosition orbit instant ma ea ta pv
  where
    ea = meanToEccentricAnomaly orbit ma
    instant = instantFromMeanAnomalyAndRevs orbit ma n
    ta = eccentricToTrueAnomaly orbit ea
    pv = trueAnomalyToPositionVector orbit ta

orbitPositionFromEccentricAnomaly :: OrbitClass t => t -- ^ orbit
                                                  -> Dimensionless Double -- ^ eccentric anomaly
                                                  -> Integer -- ^ full revolutions since epoch
                                                  -> OrbitPosition t
orbitPositionFromEccentricAnomaly orbit ea n = OrbitPosition orbit instant ma ea ta pv
  where
    ta = eccentricToTrueAnomaly orbit ea
    pv = trueAnomalyToPositionVector orbit ta
    ma = eccentricToMeanAnomaly orbit ea
    instant = instantFromMeanAnomalyAndRevs orbit ma n

orbitPositionFromTrueAnomaly :: OrbitClass t => t -- ^ orbit
                                             -> Dimensionless Double -- ^ true anomaly
                                             -> Integer -- ^ full revolutions since epoch
                                             -> OrbitPosition t
orbitPositionFromTrueAnomaly orbit ta n = OrbitPosition orbit instant ma ea ta pv
  where
    pv = trueAnomalyToPositionVector orbit ta
    ea = trueToEccentricAnomaly orbit ta
    ma = eccentricToMeanAnomaly orbit ea
    instant = instantFromMeanAnomalyAndRevs orbit ma n

bodyPosition :: KerbalInstant -> BodyId -> Length (V3 Double)
bodyPosition t b = maybe dimZero orbitPosition (b ^? bodyOrbit)
  where
    orbitPosition o = positionVector (orbitPositionFromInstant o t) ^+^ bodyPosition t (o ^. orbitBody . bodyId)

