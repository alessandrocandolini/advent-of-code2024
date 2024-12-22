{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Day6 where

import Control.Applicative (many, (<|>))
import Control.Comonad (extend, extract)
import Control.Comonad.Representable.Store
import Data.Array (Array, Ix, array, assocs, bounds, inRange, (!))
import Data.Bifunctor (first)
import Data.Distributive
import Data.Either.Combinators (maybeToRight)
import Data.Functor (($>))
import Data.Functor.Rep
import Data.List (nub, unfoldr)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Safe (headMay)
import Text.Megaparsec (Parsec, runParser, sepBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Error (ParseErrorBundle)
import Witherable (mapMaybe)

data Error
  = ParsingError ParserError
  | WalkerMissing
  deriving (Eq, Show)

newtype X = X Int deriving (Eq, Show, Num, Ord, Ix, Enum) via Int
newtype Y = Y Int deriving (Eq, Show, Num, Ord, Ix, Enum) via Int
type Position = (X, Y)

data Cell
  = OpenSpace
  | Obstruction
  | Patrol Direction
  | OutOfbounds
  deriving (Eq, Show)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded)

directionsClockwise :: Direction -> NonEmpty Direction
directionsClockwise North = [North, East, South, West]
directionsClockwise East = [East, South, West, North]
directionsClockwise South = [South, West, North, East]
directionsClockwise West = [West, North, East, South]

moveForward :: Direction -> Position -> Position
moveForward North (x, y) = (x, y - 1)
moveForward South (x, y) = (x, y + 1)
moveForward East (x, y) = (x + 1, y)
moveForward West (x, y) = (x - 1, y)

newtype Grid a = Grid {ungrid :: Position -> a} deriving (Functor)

instance Distributive Grid where
  distribute f = Grid $ \p -> fmap (\(Grid g) -> g p) f

instance Representable Grid where
  type Rep Grid = Position
  index = ungrid
  tabulate = Grid

associatedList :: [[a]] -> [(Position, a)]
associatedList = concatMap (\(j, row) -> zipWith (\i a -> ((i, j), a)) [0 ..] row) . zip [0 ..]

dimensions :: [[a]] -> (X, Y)
dimensions as = (numberOfColumns, numberOfRows)
 where
  numberOfRows = Y (length as)
  numberOfColumns = maybe 0 (X . length) (headMay as)

corners :: [[a]] -> ((X, Y), (X, Y))
corners as = ((0, 0), (width - 1, height - 1))
 where
  (width, height) = dimensions as

-- will throw at runtime if grid is not rectangular
-- array is also a strict function of the bounds and coordinates
listToArray :: [[a]] -> Array Position a
listToArray as = array (corners as) (associatedList as)

arrayToGrid :: a -> Array Position a -> Grid a
arrayToGrid defaultValue as = Grid g
 where
  g p
    | inRange (bounds as) p = as ! p
    | otherwise = defaultValue

type LabMap = Store Grid Cell

initialLabMap :: [[Cell]] -> Maybe LabMap
initialLabMap cells = fmap (store initialGrid . fst) initialPatrol
 where
  cellsArray = listToArray cells
  initialGrid = ungrid (arrayToGrid OutOfbounds cellsArray)
  initialPatrol = headMay (mapMaybe extractPatrolPositionAndDirection (assocs cellsArray))
  extractPatrolPositionAndDirection :: (Position, Cell) -> Maybe (Position, Direction)
  extractPatrolPositionAndDirection (p, Patrol d) = Just (p, d)
  extractPatrolPositionAndDirection _ = Nothing

patrolPosition :: LabMap -> Position
patrolPosition = pos

inspectPosition :: LabMap -> Position -> Cell
inspectPosition m p = peek p m

-- we are abusing the grid to store also the direction. As a downside, the operation has to return Maybe :(
directionFromPosition :: LabMap -> Position -> Maybe Direction
directionFromPosition m p = case inspectPosition m p of
  Patrol d -> Just d
  _ -> Nothing

patrol :: LabMap -> Maybe (Position, Direction)
patrol m = do
  let p = patrolPosition m
  d <- directionFromPosition m p
  pure (p, d)

-- TODO inefficient due to extend?
movePatrol :: LabMap -> Position -> Position -> Direction -> LabMap
movePatrol m from to d =
  seek to $
    extend (\s -> if pos s == to then Patrol d else if pos s == from then OpenSpace else extract s) m

data Move = MoveTo Position Direction | GameOver

destinations :: LabMap -> Position -> Direction -> NonEmpty (Position, Direction, Cell)
destinations m p d = fmap generate (directionsClockwise d)
 where
  generate d' =
    let
      candidate = moveForward d' p
     in
      (candidate, d', peek candidate m)

nextMove :: LabMap -> Position -> Direction -> Move
nextMove m p d =
  case N.head candidates of
    (_, _, OutOfbounds) -> GameOver
    _ -> fromMaybe GameOver (headMay (mapMaybe available (N.toList candidates)))
 where
  candidates = destinations m p d
  available (p', d', OpenSpace) = Just (MoveTo p' d')
  available (p', d', Patrol _) = Just (MoveTo p' d') -- should we allow this
  available _ = Nothing

oneStep :: LabMap -> Position -> Direction -> Maybe LabMap
oneStep m position direction =
  case nextMove m position direction of
    (MoveTo destination finalDirection) -> Just $ movePatrol m position destination finalDirection
    GameOver -> Nothing

walk :: LabMap -> [(Position, Direction)]
walk m = unfoldr step (Just m)
 where
  step :: Maybe LabMap -> Maybe ((Position, Direction), Maybe LabMap)
  step Nothing = Nothing -- No more steps to take
  step (Just next) = do
    (position, direction) <- patrol next
    let nextMap = oneStep next position direction
    Just ((position, direction), nextMap)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int deriving (Eq, Show)

logic :: T.Text -> Either Error Answer
logic = (=<<) answer . first ParsingError . parse

answer :: [[Cell]] -> Either Error Answer
answer = fmap Answer . part1

part1 :: [[Cell]] -> Either Error Int
part1 = fmap (countUnique . fmap fst) . loadAndWalk

countUnique :: (Eq a) => [a] -> Int
countUnique = length . nub

loadAndWalk :: [[Cell]] -> Either Error [(Position, Direction)]
loadAndWalk cells =
  walk
    <$> maybeToRight WalkerMissing (initialLabMap cells)

-- parsing

type Parser = Parsec Void T.Text
type ParserError = ParseErrorBundle T.Text Void

cellParser :: Parser Cell
cellParser =
  char '.' $> OpenSpace
    <|> char '#' $> Obstruction
    <|> char '^' $> Patrol North

cellsParser :: Parser [[Cell]]
cellsParser = sepBy (many cellParser) newline

parse :: T.Text -> Either ParserError [[Cell]]
parse = runParser cellsParser "input"
