module Horbits.Body.Id(BodyId(..)) where

data BodyId =
    Sun
    | Kerbin
    | Mun
    | Minmus
    | Moho
    | Eve
    | Duna
    | Ike
    | Jool
    | Laythe
    | Vall
    | Bop
    | Tylo
    | Gilly
    | Pol
    | Dres
    | Eeloo
    | Sarnus
    | Urlum
    | Neidon
    | Hale
    | Ovok
    | Slate
    | Plock
    | Tekto
    | Polta
    | Priax
    | Wal
    deriving (Bounded, Enum, Show, Eq)
