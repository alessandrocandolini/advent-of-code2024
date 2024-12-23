module Day4 where

import Data.List (tails, transpose)
import qualified Data.Text as T
import Witherable (mapMaybe)

program :: T.Text -> IO ()
program = print . logic

data Answer = Answer Int Int deriving (Eq, Show)

answer :: Grid Char -> Answer
answer = Answer <$> part1 <*> part2

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
    <$> concatMap (mapMaybe p . tails) . rows
    <*> concatMap (mapMaybe p . tails) . columns
    <*> mapMaybe p . diagonals -- no need for tails: we already precalculate partial diagonals
    <*> mapMaybe p . backwardDiagonals

-- Part 2 --

type Square3 a = ((a, a, a), (a, a, a), (a, a, a))

squares3 :: [[a]] -> [Square3 a]
squares3 = concatMap groupColumns3 . groupRows3
 where
  groupRows3 :: [[a]] -> [[(a, a, a)]]
  groupRows3 = fmap toPair3 . tails
  toPair3 :: [[a]] -> [(a, a, a)]
  toPair3 (a1 : a2 : a3 : _) = zip3 a1 a2 a3
  toPair3 _ = []
  groupColumns3 = mapMaybe extractFirst3 . tails
  extractFirst3 :: [a] -> Maybe (a, a, a)
  extractFirst3 (a1 : a2 : a3 : _) = Just (a1, a2, a3)
  extractFirst3 _ = Nothing

isSquare3Matching :: Square3 (a -> Bool) -> Square3 a -> Bool
isSquare3Matching ((p1, p2, p3), (p4, p5, p6), (p7, p8, p9)) ((a1, a2, a3), (a4, a5, a6), (a7, a8, a9)) = p1 a1 && p2 a2 && p3 a3 && p4 a4 && p5 a5 && p6 a6 && p7 a7 && p8 a8 && p9 a9

isSquare3MatchingAny :: [Square3 (a -> Bool)] -> Square3 a -> Bool
isSquare3MatchingAny expected input = any ((flip isSquare3Matching) input) expected

validSquare3 :: [Square3 (Char -> Bool)]
validSquare3 =
  [ ((is 'M', anything, is 'M'), (anything, is 'A', anything), (is 'S', anything, is 'S'))
  , ((is 'S', anything, is 'S'), (anything, is 'A', anything), (is 'M', anything, is 'M'))
  , ((is 'M', anything, is 'S'), (anything, is 'A', anything), (is 'M', anything, is 'S'))
  , ((is 'S', anything, is 'M'), (anything, is 'A', anything), (is 'S', anything, is 'M'))
  ]
 where
  is :: Char -> Char -> Bool
  is = (==)
  anything :: Char -> Bool
  anything = const True

part2 :: Grid Char -> Int
part2 = length . filter (isSquare3MatchingAny validSquare3) . squares3 . grid

-- Input parsing --

parse :: String -> Maybe Xmas
parse ('X' : 'M' : 'A' : 'S' : _) = Just Xmas
parse ('S' : 'A' : 'M' : 'X' : _) = Just Samx
parse _ = Nothing

parseGrid :: T.Text -> Grid Char
parseGrid = Grid . fmap T.unpack . T.lines
