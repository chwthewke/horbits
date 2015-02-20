module Horbits.SolarSystem (bodiesList, bodiesTree) where

import           Control.Lens
import           Data.List                            (partition, sortBy)
import           Data.Tree
import           Horbits.Body
import           Horbits.Orbit
import           Numeric.Units.Dimensional.TF.Prelude

bodiesList :: [Body]
bodiesList = [minBound..] ^.. traverse . fromBodyId

bodiesTree :: Forest Body
bodiesTree = foldr insert [] bodiesList
  where insert = curry $ uncurry insertToForest . withChildren

isParentBodyOf :: Body -> Body -> Bool
isParentBodyOf b b' = b' ^? bodyId . parentBodyId == Just (b ^. bodyId)

withChildren :: (Body, Forest Body) -> (Tree Body, Forest Body)
withChildren (b, bs) = (Node b $ sortBy closer cs, ds)
  where
    (cs, ds) = partition (isParentBodyOf b . rootLabel) bs

insertToForest :: Tree Body -> Forest Body -> Forest Body
insertToForest b f = if ok then fmap snd inserts else b:f
  where
    inserts = fmap (insertToParent b) f
    ok = or $ fmap fst inserts

insertToParent :: Tree Body -> Tree Body -> (Bool, Tree Body)
insertToParent t (Node b ts) = let v = isParentT t  in (v, if v then Node b (insertToSiblings t ts) else Node b ts)
  where
    isParentT (Node b' _) = b' ^? bodyId . parentBodyId == Just (b ^. bodyId)

insertToSiblings :: Tree Body -> Forest Body -> Forest Body
insertToSiblings t ts = sortBy closer (t:ts)


closer :: Tree Body -> Tree Body -> Ordering
closer (Node b _) (Node b' _) = compare (b ^. periapsis') (b' ^. periapsis')
  where
    periapsis' = bodyId . pre (bodyOrbit . periapsis) . non _0

