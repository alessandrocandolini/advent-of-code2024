module Day1 (part1, part2, parse, program) where

import Data.Bifunctor (Bifunctor (second), bimap)
import Data.List (sort)
import qualified Data.Map as M
import Data.Semigroup (Sum (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec (Parsec, optional, runParser)
import Text.Megaparsec.Char (eol, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Utils (groupBy)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int Int deriving (Eq, Show)

answer :: [(Int, Int)] -> Answer
answer = Answer <$> part1 <*> part2

logic :: T.Text -> Either ParsingError Answer
logic = fmap answer . parseAll

part1 :: (Ord a, Num a) => [(a, a)] -> a
part1 =
  getSum
    . foldMap (Sum . uncurry distance)
    . uncurry zip
    . bimap sort sort
    . unzip
 where
  distance = (abs .) . (-)

part2 :: (Integral a) => [(a, a)] -> Int
part2 =
  getSum
    . uncurry similarities
    . swap
    . second occurrences
    . unzip
 where
  similarities m = foldMap (\b -> Sum (fromIntegral b * M.findWithDefault 0 b m))

occurrences :: (Ord a) => [a] -> M.Map a Int
occurrences = fmap length . groupBy id

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

parser :: Parser (Int, Int)
parser = (,) <$> (decimal <* space) <*> (decimal <* optional eol)

parse :: T.Text -> Either ParsingError (Int, Int)
parse = runParser parser "input file"

parseAll :: T.Text -> Either ParsingError [(Int, Int)]
parseAll = traverse parse . T.lines
