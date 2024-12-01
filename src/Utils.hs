module Utils (groupBy, groupMap) where

import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Sequence as Seq

groupBy :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
groupBy key = groupMap key id

groupMap :: (Ord k) => (a -> k) -> (a -> v) -> [a] -> M.Map k [v]
groupMap key value = M.map toList . M.fromListWith (flip (<>)) . fmap (\a -> (key a, Seq.singleton (value a)))
