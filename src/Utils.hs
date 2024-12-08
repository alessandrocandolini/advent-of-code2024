module Utils (groupBy, groupMap,colorText) where

import Data.Foldable (toList)
import qualified Data.Map as M
import qualified Data.Sequence as Seq
import System.Console.ANSI (
  Color (..),
  ColorIntensity (Dull),
  ConsoleIntensity (BoldIntensity),
  ConsoleLayer (Foreground),
  SGR (Reset, SetColor, SetConsoleIntensity),
  setSGRCode,
 )

groupBy :: (Ord k) => (a -> k) -> [a] -> M.Map k [a]
groupBy key = groupMap key id

groupMap :: (Ord k) => (a -> k) -> (a -> v) -> [a] -> M.Map k [v]
groupMap key value = M.map toList . M.fromListWith (flip (<>)) . fmap (\a -> (key a, Seq.singleton (value a)))

colorText :: Color -> String -> String
colorText color text =
  setSGRCode [SetColor Foreground Dull color, SetConsoleIntensity BoldIntensity] ++ text ++ setSGRCode [Reset]

