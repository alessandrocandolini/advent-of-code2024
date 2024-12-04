module Day4 where

import Data.List (tails, transpose)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Witherable (mapMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int deriving (Eq, Show)

answer :: Grid Char -> Answer
answer = Answer . part1

logic :: T.Text -> Answer
logic = answer . parseGrid

part1 :: Grid Char -> Int
part1 = countAll . findOccurrences parse

data Xmas = Xmas | Samx deriving (Eq, Show)

newtype Grid a = Grid
  { grid :: [[a]]
  }
  deriving (Eq, Show)

data Occurrences a = Occurrences
  { horizontal :: [a]
  , vertical :: [a]
  , diagonal :: [a]
  , backwardDiagonal :: [a]
  }
  deriving (Eq, Show)

countAll :: Occurrences a -> Int
countAll o =
  length (horizontal o)
    + length (vertical o)
    + length (diagonal o)
    + length (backwardDiagonal o)

rows :: Grid a -> [[a]]
rows = grid

columns :: Grid a -> [[a]]
columns = transpose . grid

diagonals :: Grid a -> [[a]]
diagonals =
  concatMap
    ( (columns . Grid)
        . fmap (uncurry drop)
        . zip [0 ..]
    )
    . tails
    . grid

backwardDiagonals :: Grid a -> [[a]]
backwardDiagonals = diagonals . Grid . fmap reverse . grid

findOccurrences :: ([a] -> Maybe b) -> Grid a -> Occurrences b
findOccurrences p =
  Occurrences
    <$> (concatMap (mapMaybe p . tails) . rows)
    <*> (concatMap (mapMaybe p . tails) . columns)
    <*> mapMaybe p . diagonals -- no need for tails: we already precalculate partial diagonals
    <*> mapMaybe p . backwardDiagonals

parse :: String -> Maybe Xmas
parse ('X' : 'M' : 'A' : 'S' : _) = Just Xmas
parse ('S' : 'A' : 'M' : 'X' : _) = Just Samx
parse _ = Nothing

parseGrid :: T.Text -> Grid Char
parseGrid = Grid . fmap T.unpack . T.lines
