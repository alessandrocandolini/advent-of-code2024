module Day4 where

import Data.List (tails, transpose, inits)
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
  }
  deriving (Eq, Show)

countAll :: Occurrences a -> Int
countAll o = length (horizontal o) + length (vertical o) + length (diagonal o)

horizontalSlices :: Grid a -> [[a]]
horizontalSlices = concatMap tails . grid

verticalSlices :: Grid a -> [[a]]
verticalSlices = horizontalSlices . Grid . transpose . grid

diagonalSlices :: Grid a -> [[a]]
diagonalSlices (Grid g) = [[]]


findOccurrences :: ([a] -> Maybe b) -> Grid a -> Occurrences b
findOccurrences p =
  Occurrences
    <$> (mapMaybe p . horizontalSlices)
    <*> (mapMaybe p . verticalSlices)
    <*> (mapMaybe p . diagonalSlices)

parse :: String -> Maybe Xmas
parse ('X' : 'M' : 'A' : 'S' : _) = Just Xmas
parse ('S' : 'A' : 'M' : 'X' : _) = Just Samx
parse _ = Nothing

parseGrid :: T.Text -> Grid Char
parseGrid = Grid . fmap T.unpack . T.lines
