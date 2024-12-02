{-# LANGUAGE DerivingVia #-}

module Day2 (Answer (..), Report (..), Level (..), program, logic, monotonic, boundPairwiseDistance, parseLines, parseLine, part1, part2, isReportSafe1, isReportSafe2, allButOne) where

import Control.Applicative (many)
import Data.List (group, inits, tails)
import qualified Data.List.NonEmpty as N
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec (Parsec, optional, runParser)
import Text.Megaparsec.Char (eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int Int deriving (Eq, Show)

answer :: [Report] -> Answer
answer = Answer <$> part1 <*> part2

newtype Level = Level Int
  deriving (Eq, Show)
  deriving (Ord, Num) via Int

newtype Report = Report
  { levels :: [Level]
  }
  deriving (Eq, Show)

logic :: T.Text -> Either ParsingError Answer
logic = fmap answer . parseLines

part1 :: [Report] -> Int
part1 = count isReportSafe1

part2 :: [Report] -> Int
part2 = count isReportSafe2

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

monotonic :: (Ord a) => [a] -> Bool
monotonic = isNullOrSingleton . group . compareWithNext
 where
  isNullOrSingleton [] = True
  isNullOrSingleton [_] = True
  isNullOrSingleton (_ : _) = False
  compareWithNext :: (Ord b) => [b] -> [Ordering]
  compareWithNext bs = zipWith compare bs (tail bs)

distance :: (Num a) => a -> a -> a
distance = (abs .) . (-)

pairwiseDistances :: (Num a) => [a] -> [a]
pairwiseDistances as = uncurry distance <$> zip as (tail as)

boundPairwiseDistance :: (Ord a, Num a) => [a] -> Bool
boundPairwiseDistance = maybe True ((<= 3) . maximum) . N.nonEmpty . pairwiseDistances

isSafe1 :: (Ord a, Num a) => [a] -> Bool
isSafe1 as = monotonic as && boundPairwiseDistance as

isReportSafe1 :: Report -> Bool
isReportSafe1 = isSafe1 . levels

allButOne :: [a] -> [[a]]
allButOne as = zipWith (++) (inits as) (drop 1 (tails as))

isSafe2 :: (Ord a, Num a) => [a] -> Bool
isSafe2 = any isSafe1 . allButOne

isReportSafe2 :: Report -> Bool
isReportSafe2 = isSafe2 . levels

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

parser :: Parser Report
parser = Report <$> many (Level <$> (decimal <* space)) <* optional eol

parseLine :: T.Text -> Either ParsingError Report
parseLine = runParser parser "input file"

parseLines :: T.Text -> Either ParsingError [Report]
parseLines = traverse parseLine . T.lines
