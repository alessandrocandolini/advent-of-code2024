{-# LANGUAGE DerivingVia #-}

module Day2 (Answer (..), Report (..), Level (..), program, logic, monotonic, boundPairwiseDistance, parseAll, parse, part1, part2, isReportSafe1, isReportSafe2, allButOne) where

import Control.Applicative (many)
import Data.List (inits, tails)
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
logic = fmap answer . parseAll

part1 :: [Report] -> Int
part1 = count isReportSafe1

part2 :: [Report] -> Int
part2 = count isReportSafe2

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

monotonic :: (Ord a) => [a] -> Bool
monotonic as = all (uncurry (>)) as' || all (uncurry (<)) as'
 where
  as' = zip as (tail as) -- safe on empty lists due to lazy evaluation

distance :: (Num a) => a -> a -> a
distance = (abs .) . (-)

pairwiseDistance :: (Num a) => [a] -> [a]
pairwiseDistance as = uncurry distance <$> zip as (tail as)

boundPairwiseDistance :: (Ord a, Num a) => [a] -> Bool
boundPairwiseDistance [] = True
boundPairwiseDistance [_] = True
boundPairwiseDistance as = maximum (pairwiseDistance as) <= 3

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

parse :: T.Text -> Either ParsingError Report
parse = runParser parser "input file"

parseAll :: T.Text -> Either ParsingError [Report]
parseAll = traverse parse . T.lines
