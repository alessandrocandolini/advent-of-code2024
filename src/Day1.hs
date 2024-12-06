module Day1 (logic, part1, part2, parseAll, program, Answer (..)) where

import Data.Bifunctor (Bifunctor (second), bimap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (foldl', sort)
import Data.Semigroup (Sum (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple (swap)
import Data.Void (Void)
import Text.Megaparsec (Parsec, runParser, sepBy)
import Text.Megaparsec.Char (newline, space)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)

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

part2 :: (Integral a, Hashable a) => [(a, a)] -> Int
part2 =
  getSum
    . uncurry similarities
    . swap
    . second occurrences
    . unzip
 where
  similarities m = foldMap (\b -> Sum (fromIntegral b * HashMap.findWithDefault 0 b m))

occurrences :: (Hashable a) => [a] -> HashMap.HashMap a Int
occurrences = foldl' (\acc x -> HashMap.insertWith (+) x 1 acc) HashMap.empty

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

parser :: Parser (Int, Int)
parser = (,) <$> (decimal <* space) <*> decimal

parserAll :: Parser [(Int, Int)]
parserAll = sepBy parser newline

parseAll :: T.Text -> Either ParsingError [(Int, Int)]
parseAll = runParser parserAll "input file"
