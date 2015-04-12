module Horbits.SolarSystem (BodyPosition(..), bodiesList, bodiesTree, bodyPosition) where

import           Control.Lens
import           Data.List                   (sortBy)
import           Data.Tree

import           Horbits.Body
import           Horbits.Dimensional.Prelude
import           Horbits.Orbit

data BodyPosition = Star BodyId | Planet Integer BodyId BodyId | Moon Integer BodyId BodyId
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
    apoapsis' = pre (bodyOrbit . orbitApoapsis) . non _0

childPosition :: BodyPosition -> Integer -> BodyId -> BodyPosition
childPosition (Star b) = flip Planet b
childPosition (Planet _ _ b) = flip Moon b
childPosition (Moon _ _ b) = flip Moon b

toBodyId :: BodyPosition -> BodyId
toBodyId (Star b) = b
toBodyId (Planet _ _ b) = b
toBodyId (Moon _ _ b) = b

bodiesHierarchy :: Tree BodyPosition
bodiesHierarchy = unfoldTree satellites (Star Sun)
  where
    satellites bp = (bp,
        zipWith (childPosition bp) [1 ..] .
        sortBy closer .
        filter (toBodyId bp `isParentBodyOf`) $
        [minBound..])

