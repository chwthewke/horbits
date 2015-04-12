module Horbits.Body (bodyUiColor, getBody, fromBodyId, module X)
  where

import           Control.Lens            hiding ((*~), _2, _3, _4)

import           Horbits.Body.Atmosphere as X
import           Horbits.Body.Body       as X
import           Horbits.Body.Color      as X
import           Horbits.Body.Data       as X
import           Horbits.Body.Id         as X

bodyUiColor :: Fold BodyId (RgbaColor Float)
bodyUiColor = to getColor . traverse

-- Data
getBody :: BodyId -> Body
getBody bId = Body bId (_bodyName bId) mu r t soi atm
  where
    soi = getSphereOfInfluence bId
    (r, t, mu) = getPhysicalAttrs bId
    atm = getAtmosphere bId

_bodyName :: BodyId -> String
_bodyName Sun = "Kerbol"
_bodyName b = show b


fromBodyId :: Getter BodyId Body
fromBodyId = to getBody

