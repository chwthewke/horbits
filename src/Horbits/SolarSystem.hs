module Horbits.SolarSystem (bodiesList, bodiesTree, bodyPosition) where

import           Control.Lens
import           Data.List                            (sortBy)
import           Data.Tree
import           Horbits.Body
import           Horbits.Orbit
import           Numeric.Units.Dimensional.TF.Prelude

data BodyPosition = Sun BodyId | Planet Int BodyId BodyId | Moon Int BodyId BodyId
    deriving (Show, Eq)

bodiesList :: [Body]
bodiesList = [minBound..] ^.. traverse . fromBodyId

bodiesTree :: Tree Body
bodiesTree = fmap (view fromBodyId . toBodyId) bodiesHierarchy

bodyPosition :: BodyId -> BodyPosition
bodyPosition bId = head . filter ((bId ==) . toBodyId) . flatten $ bodiesHierarchy

isParentBodyOf :: BodyId -> BodyId -> Bool
isParentBodyOf b b' = b' ^? parentBodyId == Just b

closer :: BodyId -> BodyId -> Ordering
closer b b' = compare (b ^. apoapsis') (b' ^. apoapsis')
  where
    apoapsis' = pre (bodyOrbit . apoapsis) . non _0

childPosition :: BodyPosition -> Int -> BodyId -> BodyPosition
childPosition (Sun b) = flip Planet b
childPosition (Planet _ _ b) = flip Moon b
childPosition (Moon _ _ b) = flip Moon b

toBodyId :: BodyPosition -> BodyId
toBodyId (Sun b) = b
toBodyId (Planet _ _ b) = b
toBodyId (Moon _ _ b) = b

bodiesHierarchy :: Tree BodyPosition
bodiesHierarchy = unfoldTree satellites (Sun Kerbol)
  where
    satellites bp = (bp,
        zipWith (childPosition bp) [1 ..] .
        sortBy closer .
        filter (toBodyId bp `isParentBodyOf`) $
        [minBound..])

