{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingVia #-}
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
  | Guard Direction
  | OutOfbounds
  deriving (Eq, Show)

data Direction
  = North
  | East
  | South
  | West
  deriving (Eq, Show, Enum, Bounded)

directionsClockwise :: Direction -> [Direction]
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

-- will throw at runtime if grid is not rectangular
-- array is also a strict function of the bounds and coordinates
listToArray :: [[a]] -> Array Position a
listToArray as = array corners (associatedList as)
 where
  (width, height) = dimensions as
  corners = ((0, 0), (width - 1, height - 1))

arrayToGrid :: a -> Array Position a -> Grid a
arrayToGrid defaultValue as = Grid g
 where
  g p
    | inRange (bounds as) p = as ! p
    | otherwise = defaultValue

initialLabMap :: [[Cell]] -> Maybe (LabMap, Position, Direction)
initialLabMap cells = fmap (\p -> (store initialGrid (fst p), fst p, snd p)) initialHead
 where
  cellsArray = listToArray cells
  initialGrid = ungrid (arrayToGrid OutOfbounds cellsArray)
  initialHead = headMay (mapMaybe headPositionAndDirection (assocs cellsArray))
  headPositionAndDirection :: (Position, Cell) -> Maybe (Position, Direction)
  headPositionAndDirection (p, Guard d) = Just (p, d)
  headPositionAndDirection _ = Nothing

type LabMap = Store Grid Cell

-- algebra

currentGuard :: LabMap -> Maybe (Position, Direction)
currentGuard = undefined

checkAvailability :: LabMap -> Position -> Cell
checkAvailability = undefined

moveGuard :: LabMap -> Position -> Direction -> LabMap
moveGuard = undefined

data Move = MoveTo Position Direction | GameOver

nextMove :: LabMap -> Move
nextMove game =
  let
    position :: Position
    position = pos game
    direction = fromMaybe South (currentDirection game) -- TODO
    newPos :: Direction -> (Position, Direction, Cell)
    newPos d =
      let
        candidate = moveForward d position
       in
        (candidate, d, peek candidate game)
    candidates :: [(Position, Direction, Cell)]
    candidates = fmap newPos (directionsClockwise direction)
    res :: Move
    res = case headMay candidates of
      Just (_, _, OutOfbounds) -> GameOver
      _ -> fromMaybe GameOver (headMay (mapMaybe available candidates))
    available (p, d, OpenSpace) = Just (MoveTo p d)
    available (p, d, Guard _) = Just (MoveTo p d) -- should we allow this
    available _ = Nothing
   in
    res

oneStep :: LabMap -> Maybe LabMap
oneStep game = game'
 where
  current = pos game
  move = nextMove game
  game' :: Maybe LabMap
  game' = case move of
    (MoveTo p d) -> Just $ update game d current p
    GameOver -> Nothing

currentDirection :: LabMap -> Maybe Direction
currentDirection game = case peek (pos game) game of
  Guard d -> Just d
  _ -> Nothing

update :: LabMap -> Direction -> Position -> Position -> LabMap
update game direction old new =
  seek new $
    extend (\s -> if pos s == new then Guard direction else if pos s == old then OpenSpace else extract s) game

walk :: LabMap -> [(Position, Maybe Direction)]
walk = unfoldr step
 where
  step :: LabMap -> Maybe ((Position, Maybe Direction), LabMap)
  step = fmap (\g -> ((pos g, currentDirection g), g)) . oneStep

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int deriving (Eq, Show)

logic :: T.Text -> Either Error Answer
logic = (=<<) answer . (first ParsingError . parse)

answer :: [[Cell]] -> Either Error Answer
answer = fmap Answer . part1

part1 :: [[Cell]] -> Either Error Int
part1 = fmap (countUnique . fmap fst) . loadAndWalk

countUnique :: (Eq a) => [a] -> Int
countUnique = length . nub

loadAndWalk :: [[Cell]] -> Either Error [(Position, Maybe Direction)]
loadAndWalk cells = do
  (g, p, d) <- maybeToRight WalkerMissing (initialLabMap cells)
  return $ (p, Just d) : walk g

-- parsing

type Parser = Parsec Void T.Text
type ParserError = ParseErrorBundle T.Text Void

cellParser :: Parser Cell
cellParser =
  (char '.' $> OpenSpace)
    <|> (char '#' $> Obstruction)
    <|> (char '^' $> Guard North)

cellsParser :: Parser [[Cell]]
cellsParser = sepBy (many cellParser) newline

parse :: T.Text -> Either ParserError [[Cell]]
parse = runParser cellsParser "input"
