module Day3 where

import Control.Applicative (Alternative ((<|>)), many)
import Data.Foldable (Foldable (foldl'))
import Data.Semigroup (Sum (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, runParser, skipManyTill)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Witherable (mapMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int Int deriving (Eq, Show)

answer :: [Operation] -> Answer
answer = Answer <$> part1 <*> part2

logic :: T.Text -> Either ParsingError Answer
logic = fmap answer . parseAll

data Operation
  = Mul Int Int
  | Do
  | Dont
  deriving (Eq, Show)

data ArithmeticOperation = Multiply Int Int deriving (Eq, Show)

eval :: ArithmeticOperation -> Int
eval (Multiply a b) = a * b

evalAll :: [ArithmeticOperation] -> Int
evalAll = getSum . foldMap (Sum . eval)

part1 :: [Operation] -> Int
part1 = evalAll . mapMaybe ignoreDoDontOperations
 where
  ignoreDoDontOperations (Mul a b) = Just (Multiply a b)
  ignoreDoDontOperations _ = Nothing

part2 :: [Operation] -> Int
part2 = evalAll . includeDoDontOperations

includeDoDontOperations :: [Operation] -> [ArithmeticOperation]
includeDoDontOperations = reverse . snd . foldl' accumulate (True, [])
 where
  accumulate :: (Bool, [ArithmeticOperation]) -> Operation -> (Bool, [ArithmeticOperation])
  accumulate (True, accumulated) (Mul a b) = (True, Multiply a b : accumulated)
  accumulate (False, accumulated) (Mul _ _) = (False, accumulated)
  accumulate (_, accumulated) Dont = (False, accumulated)
  accumulate (_, accumulated) Do = (True, accumulated)

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

parser :: Parser [Operation]
parser = many (try (skipManyTill anySingle (try operationParser)))

operationParser :: Parser Operation
operationParser =
  (Dont <$ string "don't()")
    <|> (Do <$ string "do()")
    <|> (Mul <$> (string "mul(" *> decimal <* char ',') <*> (decimal <* char ')'))

parseAll :: T.Text -> Either ParsingError [Operation]
parseAll = runParser parser "input file"
