module Horbits.OrbitSample where

import           Control.Lens                      ((^.))
import           Horbits.DimLin
import           Horbits.Orbit
import           Horbits.Types
import           Numeric.Units.Dimensional.Prelude
import           Prelude                           hiding (negate, pi, sqrt,
                                                    (*))
import           Test.QuickCheck

data OrbitSample = OrbitSample { desc  :: String
                               , orbit :: Orbit
                               , sma   :: Length Double
                               , e     :: Dimensionless Double
                               , raan  :: Dimensionless Double
                               , incl  :: Dimensionless Double
                               , arg   :: Dimensionless Double
                               , ap    :: Length Double
                               , pe    :: Length Double
                               } deriving (Show)

orbitSample :: String
               -> Orbit
               -> SemiMajorAxis
               -> Eccentricity
               -> RightAscensionOfAscendingNode
               -> Inclination
               -> ArgumentOfPeriapsis
               -> Apoapsis
               -> Periapsis
               -> OrbitSample
orbitSample desc'
            orbit'
            (SemiMajorAxis sma')
            (Eccentricity e')
            (RightAscensionOfAscendingNode raan')
            (Inclination incl')
            (ArgumentOfPeriapsis arg')
            (Apoapsis ap')
            (Periapsis pe') =
  OrbitSample desc' orbit' sma' e' raan' incl' arg' ap' pe'

sampleOrbits :: [OrbitSample]
sampleOrbits = [orbitSample "Circular equatorial 100km"
                            (Orbit kerbin (v3 _0 _0 (sqrt hSq0)) (v3 _0 _0 _0) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity _0)
                            (RightAscensionOfAscendingNode _0)
                            (Inclination _0)
                            (ArgumentOfPeriapsis _0)
                            (Apoapsis $ 100000 *~ meter)
                            (Periapsis $ 100000 *~ meter),

                orbitSample "Circular equatorial retrograde 100km"
                            (Orbit kerbin (v3 _0 _0 (negate $ sqrt hSq0)) (v3 _0 _0 _0) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity _0)
                            (RightAscensionOfAscendingNode _0)
                            (Inclination pi)
                            (ArgumentOfPeriapsis _0)
                            (Apoapsis $ 100000 *~ meter)
                            (Periapsis $ 100000 *~ meter),

                orbitSample "Elliptical (e = 0.2) equatorial with arg.pe = 0"
                            (Orbit kerbin (v3 _0 _0 (sqrt $ 0.96 *. hSq0)) (v3 (0.2 *~ one) _0 _0) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity $ 0.2 *~ one)
                            (RightAscensionOfAscendingNode _0)
                            (Inclination _0)
                            (ArgumentOfPeriapsis _0)
                            (Apoapsis $ 240000 *~ meter)
                            (Periapsis $ (-40000) *~ meter),

                orbitSample "Circular 45° incl, raan = 0°"
                            (Orbit kerbin (v3 _0 (negate . sqrt $ 0.5 *. hSq0) (sqrt $ 0.5 *. hSq0)) (v3 _0 _0 _0) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity _0)
                            (RightAscensionOfAscendingNode _0)
                            (Inclination $ 0.25 *. pi)
                            (ArgumentOfPeriapsis _0)
                            (Apoapsis $ 100000 *~ meter)
                            (Periapsis $ 100000 *~ meter),

                orbitSample "Circular 45° incl, raan = 45°"
                            (Orbit kerbin (v3 (sqrt $ 0.25 *. hSq0) (negate . sqrt $ 0.25 *. hSq0) (sqrt $ 0.5 *. hSq0))
                              (v3 _0 _0 _0) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity _0)
                            (RightAscensionOfAscendingNode $ 0.25 *. pi)
                            (Inclination $ 0.25 *. pi)
                            (ArgumentOfPeriapsis _0)
                            (Apoapsis $ 100000 *~ meter)
                            (Periapsis $ 100000 *~ meter),
                            
                orbitSample "Elliptical (e = 0.2) 45° incl, raan = arg. pe = 0"
                            (Orbit kerbin (v3 _0 (negate . sqrt $ 0.48 *. hSq0) (sqrt $ 0.48 *. hSq0))
                              (v3 (0.2 *~ one) _0 _0) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity $ 0.2 *~ one)
                            (RightAscensionOfAscendingNode _0)
                            (Inclination $ 0.25 *. pi)
                            (ArgumentOfPeriapsis _0)
                            (Apoapsis $ 240000 *~ meter)
                            (Periapsis $ (-40000) *~ meter),
                            
                orbitSample "Elliptical (e = 0.2) 45° incl, raan = 0°, arg. pe = 90°"
                            (Orbit kerbin (v3 _0 (negate . sqrt $ 0.48 *. hSq0) (sqrt $ 0.48 *. hSq0))
                              (v3 _0 (0.2 *. sqrt (0.5 *~ one)) (0.2 *. sqrt (0.5 *~ one))) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity $ 0.2 *~ one)
                            (RightAscensionOfAscendingNode _0)
                            (Inclination $ 0.25 *. pi)
                            (ArgumentOfPeriapsis $ 0.5 *. pi)
                            (Apoapsis $ 240000 *~ meter)
                            (Periapsis $ (-40000) *~ meter),
                            
                orbitSample "Elliptical (e = 0.2) 45° incl, raan = 45°, arg.pe = 0°"
                            (Orbit kerbin (v3 (sqrt $ 0.24 *. hSq0) (negate . sqrt $ 0.24 *. hSq0) (sqrt $ 0.48 *. hSq0))
                              (v3 (0.2 *. sqrt (0.5 *~ one)) (0.2 *. sqrt (0.5 *~ one)) _0) _0)
                            (SemiMajorAxis $ 700000 *~ meter)
                            (Eccentricity $ 0.2 *~ one)
                            (RightAscensionOfAscendingNode $ 0.25 *. pi)
                            (Inclination $ 0.25 *. pi)
                            (ArgumentOfPeriapsis _0)
                            (Apoapsis $ 240000 *~ meter)
                            (Periapsis $ (-40000) *~ meter)

                           ]
  where hSq0 = 700000 *~ meter * kerbin ^. gravitationalParam
        kerbin = getBody Kerbin

genSampleOrbits :: Gen OrbitSample
genSampleOrbits = elements sampleOrbits
