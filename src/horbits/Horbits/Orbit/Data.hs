module Horbits.Orbit.Data(bodyOrbit) where

import           Control.Lens                hiding ((*~))

import           Horbits.Body
import           Horbits.Dimensional.Prelude
import           Horbits.Orbit.Class

_bodyOrbit :: BodyId -> Maybe Orbit
_bodyOrbit Sun    = Nothing
_bodyOrbit Kerbin = Just $ classical Sun    (13599840256 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Mun    = Just $ classical Kerbin (12000000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (1.70 *~ radian)
_bodyOrbit Minmus = Just $ classical Kerbin (47000000 *~ meter)
                                            (0.000000 *~ one)
                                            (78.00 *~ degree)
                                            (6.00 *~ degree)
                                            (38.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Moho   = Just $ classical Sun    (5263138304 *~ meter)
                                            (0.200000 *~ one)
                                            (70.00 *~ degree)
                                            (7.00 *~ degree)
                                            (15.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Eve    = Just $ classical Sun    (9832684544 *~ meter)
                                            (0.010000 *~ one)
                                            (15.00 *~ degree)
                                            (2.10 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Duna   = Just $ classical Sun    (20726155264 *~ meter)
                                            (0.051000 *~ one)
                                            (135.50 *~ degree)
                                            (0.06 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Ike    = Just $ classical Duna   (3200000 *~ meter)
                                            (0.030000 *~ one)
                                            (0.00 *~ degree)
                                            (0.20 *~ degree)
                                            (0.00 *~ degree)
                                            (1.70 *~ radian)
_bodyOrbit Jool   = Just $ classical Sun    (68773560320 *~ meter)
                                            (0.050000 *~ one)
                                            (52.00 *~ degree)
                                            (1.30 *~ degree)
                                            (0.00 *~ degree)
                                            (0.10 *~ radian)
_bodyOrbit Laythe = Just $ classical Jool   (27184000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Vall   = Just $ classical Jool   (43152000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Bop    = Just $ classical Jool   (128500000 *~ meter)
                                            (0.235000 *~ one)
                                            (10.00 *~ degree)
                                            (15.00 *~ degree)
                                            (25.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Tylo   = Just $ classical Jool   (68500000 *~ meter)
                                            (0.000000 *~ one)
                                            (0.00 *~ degree)
                                            (0.03 *~ degree)
                                            (0.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Gilly  = Just $ classical Eve    (31500000 *~ meter)
                                            (0.550000 *~ one)
                                            (80.00 *~ degree)
                                            (12.00 *~ degree)
                                            (10.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Pol    = Just $ classical Jool   (179890000 *~ meter)
                                            (0.170850 *~ one)
                                            (2.00 *~ degree)
                                            (4.25 *~ degree)
                                            (15.00 *~ degree)
                                            (0.90 *~ radian)
_bodyOrbit Dres   = Just $ classical Sun    (40839348203 *~ meter)
                                            (0.145000 *~ one)
                                            (280.00 *~ degree)
                                            (5.00 *~ degree)
                                            (90.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Eeloo  = Just $ classical Sarnus (19105978 *~ meter)
                                            (0.003400 *~ one)
                                            (55.00 *~ degree)
                                            (2.30 *~ degree)
                                            (260.00 *~ degree)
                                            (3.14 *~ radian)
_bodyOrbit Sarnus = Just $ classical Sun    (125798522368 *~ meter)
                                            (0.053400 *~ one)
                                            (184.00 *~ degree)
                                            (2.02 *~ degree)
                                            (0.00 *~ degree)
                                            (2.88 *~ radian)
_bodyOrbit Urlum  = Just $ classical Sun    (254317012787 *~ meter)
                                            (0.045215 *~ one)
                                            (61.00 *~ degree)
                                            (0.64 *~ degree)
                                            (0.00 *~ degree)
                                            (5.60 *~ radian)
_bodyOrbit Neidon = Just $ classical Sun    (409355191706 *~ meter)
                                            (0.012757 *~ one)
                                            (259.00 *~ degree)
                                            (1.27 *~ degree)
                                            (0.00 *~ degree)
                                            (2.27 *~ radian)
_bodyOrbit Hale   = Just $ classical Sarnus (10488231 *~ meter)
                                            (0.000000 *~ one)
                                            (55.00 *~ degree)
                                            (1.00 *~ degree)
                                            (0.00 *~ degree)
                                            (0.00 *~ radian)
_bodyOrbit Ovok   = Just $ classical Sarnus (12169413 *~ meter)
                                            (0.010000 *~ one)
                                            (55.00 *~ degree)
                                            (1.50 *~ degree)
                                            (0.00 *~ degree)
                                            (1.72 *~ radian)
_bodyOrbit Slate  = Just $ classical Sarnus (42592946 *~ meter)
                                            (0.040000 *~ one)
                                            (55.00 *~ degree)
                                            (2.30 *~ degree)
                                            (0.00 *~ degree)
                                            (1.10 *~ radian)
_bodyOrbit Plock  = Just $ classical Sun    (535833706086 *~ meter)
                                            (0.260000 *~ one)
                                            (260.00 *~ degree)
                                            (6.15 *~ degree)
                                            (50.00 *~ degree)
                                            (0.00 *~ radian)
_bodyOrbit Tekto  = Just $ classical Sarnus (97355304 *~ meter)
                                            (0.028000 *~ one)
                                            (55.00 *~ degree)
                                            (9.40 *~ degree)
                                            (0.00 *~ degree)
                                            (2.10 *~ radian)
_bodyOrbit Polta  = Just $ classical Urlum  (11727895 *~ meter)
                                            (0.001500 *~ one)
                                            (40.00 *~ degree)
                                            (2.45 *~ degree)
                                            (60.00 *~ degree)
                                            (1.52 *~ radian)
_bodyOrbit Priax  = Just $ classical Urlum  (11727895 *~ meter)
                                            (0.001500 *~ one)
                                            (40.00 *~ degree)
                                            (2.50 *~ degree)
                                            (0.00 *~ degree)
                                            (1.52 *~ radian)
_bodyOrbit Wal    = Just $ classical Urlum  (33776834 *~ meter)
                                            (0.023000 *~ one)
                                            (40.00 *~ degree)
                                            (1.90 *~ degree)
                                            (0.00 *~ degree)
                                            (2.96 *~ radian)

bodyOrbit :: Fold BodyId Orbit
bodyOrbit = folding _bodyOrbit

classical :: BodyId
               -> Length Double
               -> Dimensionless Double
               -> Dimensionless Double
               -> Dimensionless Double
               -> Dimensionless Double
               -> Dimensionless Double
               -> Orbit
classical = Orbit
