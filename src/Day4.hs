module Day4 where

import Data.List (inits, tails, transpose)
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

horizontalTails :: [[a]] -> [[a]]
horizontalTails = concatMap tails

verticalTails :: [[a]] -> [[a]]
verticalTails = horizontalTails . transpose

rotateDiagonals :: [[a]] -> [[[a]]]
rotateDiagonals =
  filter (not . null)
    . fmap
      ( filter (not . null)
          . fmap (uncurry drop)
          . zip [0 ..]
      )
    . tails

diagonalTails :: [[a]] -> [[a]]
diagonalTails = concatMap (filter (not . null) . verticalTails) . rotateDiagonals

rotateBackwardDiagonals :: [[a]] -> [[[a]]]
rotateBackwardDiagonals = rotateDiagonals . fmap reverse

backwardDiagonalTails :: [[a]] -> [[a]]
backwardDiagonalTails = concatMap verticalTails . rotateBackwardDiagonals

findOccurrences :: ([a] -> Maybe b) -> Grid a -> Occurrences b
findOccurrences p =
  Occurrences
    <$> mapMaybe p . horizontalTails . grid
    <*> mapMaybe p . verticalTails . grid
    <*> mapMaybe p . diagonalTails . grid
    <*> mapMaybe p . backwardDiagonalTails . grid

parse :: String -> Maybe Xmas
parse ('X' : 'M' : 'A' : 'S' : _) = Just Xmas
parse ('S' : 'A' : 'M' : 'X' : _) = Just Samx
parse _ = Nothing

parseGrid :: T.Text -> Grid Char
parseGrid = Grid . fmap T.unpack . T.lines
