module Horbits.Body.Data(getAtmosphere, getPhysicalAttrs, getSphereOfInfluence, getColor) where

import           Numeric.Units.Dimensional.TF.Prelude
import           Prelude                              hiding ((/), (^))

import           Horbits.Body.Atmosphere
import           Horbits.Body.Color
import           Horbits.Body.Id

mkAtmosphere :: Double -> Double -> Double -> Maybe Atmosphere
mkAtmosphere h p s = Just $ Atmosphere (h *~ meter) (p *~ kilo pascal) (s *~ meter)

mkPhysicalAttrs :: Double -> Double -> Double -> (Length Double, Time Double, GravitationalParameter Double)
mkPhysicalAttrs r t mu = (r *~ meter, t *~ second, mu *~ (meter ^ pos3 / second ^ pos2))

getAtmosphere :: BodyId -> Maybe Atmosphere
getAtmosphere Kerbin = mkAtmosphere 70000 101.32500 5000
getAtmosphere Eve    = mkAtmosphere 90000 506.62500 7000
getAtmosphere Duna   = mkAtmosphere 50000 20.26500 3000
getAtmosphere Jool   = mkAtmosphere 200000 1519.87500 10000
getAtmosphere Laythe = mkAtmosphere 50000 81.06000 4000
getAtmosphere Sarnus = mkAtmosphere 303942 1519.87500 22000
getAtmosphere Urlum  = mkAtmosphere 124340 1519.87500 9000
getAtmosphere Neidon = mkAtmosphere 110525 1519.87500 8000
getAtmosphere Tekto  = mkAtmosphere 100500 146.92125 10000
getAtmosphere _ = Nothing

getPhysicalAttrs :: BodyId -> (Length Double, Time Double, GravitationalParameter Double)
getPhysicalAttrs Sun    = mkPhysicalAttrs 261600000 432000.000 1.1723328e+018
getPhysicalAttrs Kerbin = mkPhysicalAttrs 600000 21599.912 3.5316000e+012
getPhysicalAttrs Mun    = mkPhysicalAttrs 200000 138984.377 6.5138398e+010
getPhysicalAttrs Minmus = mkPhysicalAttrs 60000 40400.000 1.7658000e+009
getPhysicalAttrs Moho   = mkPhysicalAttrs 250000 1210000.000 1.6860938e+011
getPhysicalAttrs Eve    = mkPhysicalAttrs 700000 80500.000 8.1717302e+012
getPhysicalAttrs Duna   = mkPhysicalAttrs 320000 65517.859 3.0136321e+011
getPhysicalAttrs Ike    = mkPhysicalAttrs 130000 65517.862 1.8568369e+010
getPhysicalAttrs Jool   = mkPhysicalAttrs 6000000 36000.000 2.8252800e+014
getPhysicalAttrs Laythe = mkPhysicalAttrs 500000 52980.879 1.9620000e+012
getPhysicalAttrs Vall   = mkPhysicalAttrs 300000 105962.089 2.0748150e+011
getPhysicalAttrs Bop    = mkPhysicalAttrs 65000 544507.429 2.4868349e+009
getPhysicalAttrs Tylo   = mkPhysicalAttrs 600000 211926.358 2.8252800e+012
getPhysicalAttrs Gilly  = mkPhysicalAttrs 13000 28255.000 8.2894498e+006
getPhysicalAttrs Pol    = mkPhysicalAttrs 44000 901902.624 7.2170208e+008
getPhysicalAttrs Dres   = mkPhysicalAttrs 138000 34800.000 2.1484489e+010
getPhysicalAttrs Eeloo  = mkPhysicalAttrs 210000 57904.894 7.4410815e+010
getPhysicalAttrs Sarnus = mkPhysicalAttrs 5300000 37000.000 8.2117744e+013
getPhysicalAttrs Urlum  = mkPhysicalAttrs 2177000 (-44000.000) 1.1948654e+013
getPhysicalAttrs Neidon = mkPhysicalAttrs 2145000 42500.000 1.4172721e+013
getPhysicalAttrs Hale   = mkPhysicalAttrs 6000 23551.292 7.0632000e+006
getPhysicalAttrs Ovok   = mkPhysicalAttrs 26000 29435.120 1.6578900e+008
getPhysicalAttrs Slate  = mkPhysicalAttrs 540000 192738.237 2.4315066e+012
getPhysicalAttrs Plock  = mkPhysicalAttrs 189000 2276142535.712 2.4179188e+010
getPhysicalAttrs Tekto  = mkPhysicalAttrs 280000 666040.731 1.9250673e+011
getPhysicalAttrs Polta  = mkPhysicalAttrs 220000 73004.642 9.0212760e+010
getPhysicalAttrs Priax  = mkPhysicalAttrs 74000 73004.642 3.2231736e+009
getPhysicalAttrs Wal    = mkPhysicalAttrs 370000 356819.667 4.9690593e+011

getSphereOfInfluence :: BodyId -> Maybe (Length Double)
getSphereOfInfluence Sun    = Nothing
getSphereOfInfluence Kerbin = Just $ 8.4159286e+007 *~ meter
getSphereOfInfluence Mun    = Just $ 2.4295591e+006 *~ meter
getSphereOfInfluence Minmus = Just $ 2.2474284e+006 *~ meter
getSphereOfInfluence Moho   = Just $ 9.6466630e+006 *~ meter
getSphereOfInfluence Eve    = Just $ 8.5109365e+007 *~ meter
getSphereOfInfluence Duna   = Just $ 4.7921949e+007 *~ meter
getSphereOfInfluence Ike    = Just $ 1.0495989e+006 *~ meter
getSphereOfInfluence Jool   = Just $ 2.4559852e+009 *~ meter
getSphereOfInfluence Laythe = Just $ 3.7236458e+006 *~ meter
getSphereOfInfluence Vall   = Just $ 2.4064014e+006 *~ meter
getSphereOfInfluence Bop    = Just $ 1.2210609e+006 *~ meter
getSphereOfInfluence Tylo   = Just $ 1.0856518e+007 *~ meter
getSphereOfInfluence Gilly  = Just $ 1.2612327e+005 *~ meter
getSphereOfInfluence Pol    = Just $ 1.0421389e+006 *~ meter
getSphereOfInfluence Dres   = Just $ 3.2832840e+007 *~ meter
getSphereOfInfluence Eeloo  = Just $ 1.1589078e+006 *~ meter
getSphereOfInfluence Sarnus = Just $ 2.7405011e+009 *~ meter
getSphereOfInfluence Urlum  = Just $ 2.5626107e+009 *~ meter
getSphereOfInfluence Neidon = Just $ 4.4163271e+009 *~ meter
getSphereOfInfluence Hale   = Just $ 1.5650494e+004 *~ meter
getSphereOfInfluence Ovok   = Just $ 6.4167794e+004 *~ meter
getSphereOfInfluence Slate  = Just $ 1.0421088e+007 *~ meter
getSphereOfInfluence Plock  = Just $ 4.5163371e+008 *~ meter
getSphereOfInfluence Tekto  = Just $ 8.6370052e+006 *~ meter
getSphereOfInfluence Polta  = Just $ 1.6611149e+006 *~ meter
getSphereOfInfluence Priax  = Just $ 4.3813299e+005 *~ meter
getSphereOfInfluence Wal    = Just $ 9.4667523e+006 *~ meter

getColor :: BodyId -> Maybe (RgbaColor Float)
getColor Sun    = Nothing
getColor Kerbin = Just $ RgbaColor 0.491869 0.716418 0.688153 1.000000
getColor Mun    = Just $ RgbaColor 0.523399 0.542688 0.603922 1.000000
getColor Minmus = Just $ RgbaColor 0.281820 0.227605 0.313726 0.576471
getColor Moho   = Just $ RgbaColor 0.470588 0.360784 0.270588 0.549020
getColor Eve    = Just $ RgbaColor 0.426076 0.128379 0.895522 1.000000
getColor Duna   = Just $ RgbaColor 0.641791 0.249388 0.158053 0.525490
getColor Ike    = Just $ RgbaColor 0.523399 0.542688 0.603922 1.000000
getColor Jool   = Just $ RgbaColor 0.330066 0.522388 0.078058 1.000000
getColor Laythe = Just $ RgbaColor 0.134718 0.169448 0.305970 0.541177
getColor Vall   = Just $ RgbaColor 0.433801 0.610393 0.708955 0.576471
getColor Bop    = Just $ RgbaColor 0.365672 0.314796 0.248329 0.576471
getColor Tylo   = Just $ RgbaColor 0.828358 0.669059 0.669059 1.000000
getColor Gilly  = Just $ RgbaColor 0.320896 0.250197 0.217922 0.250980
getColor Pol    = Just $ RgbaColor 0.434717 0.447761 0.338520 0.576471
getColor Dres   = Just $ RgbaColor 0.176471 0.137255 0.098039 0.525490
getColor Eeloo  = Just $ RgbaColor 0.206763 0.208268 0.208955 0.525490
getColor Sarnus = Just $ RgbaColor 0.870588 0.721569 0.529412 1.000000
getColor Urlum  = Just $ RgbaColor 0.282353 0.819608 0.800000 1.000000
getColor Neidon = Just $ RgbaColor 0.415686 0.352941 0.803922 1.000000
getColor Hale   = Just $ RgbaColor 0.941176 0.901961 0.549020 1.000000
getColor Ovok   = Just $ RgbaColor 0.690196 0.768627 0.870588 1.000000
getColor Slate  = Just $ RgbaColor 0.823529 0.705882 0.549020 1.000000
getColor Plock  = Just $ RgbaColor 0.184314 0.309804 0.309804 1.000000
getColor Tekto  = Just $ RgbaColor 0.300000 0.900000 0.650000 1.000000
getColor Polta  = Just $ RgbaColor 0.482350 0.623530 0.580390 1.000000
getColor Priax  = Just $ RgbaColor 0.419610 0.415690 0.400000 1.000000
getColor Wal    = Just $ RgbaColor 0.568630 0.478430 0.278430 1.000000

