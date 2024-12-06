module Day3 where

import Control.Applicative (Alternative ((<|>)), many)
import Data.Machine (Mealy)
import qualified Data.Machine as M
import Data.Semigroup (Sum (..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, runParser, skipManyTill)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Witherable (catMaybes, mapMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int Int deriving (Eq, Show)

answer :: [Instruction] -> Answer
answer = Answer <$> part1 <*> part2

logic :: T.Text -> Either ParsingError Answer
logic = fmap answer . parseAll

data Instruction
  = Mul Int Int
  | Do
  | Dont
  deriving (Eq, Show)

data ArithmeticOperation = Multiply Int Int deriving (Eq, Show)

eval :: ArithmeticOperation -> Int
eval (Multiply a b) = a * b

evalAll :: [ArithmeticOperation] -> Int
evalAll = getSum . foldMap (Sum . eval)

part1 :: [Instruction] -> Int
part1 = evalAll . mapMaybe ignoreDoDontOperations
 where
  ignoreDoDontOperations (Mul a b) = Just (Multiply a b)
  ignoreDoDontOperations _ = Nothing

part2 :: [Instruction] -> Int
part2 = evalAll . process

data Toggle = On | Off deriving (Eq, Show)

toggle :: Mealy Instruction (Maybe ArithmeticOperation)
toggle = M.unfoldMealy step On
 where
  step :: Toggle -> Instruction -> (Maybe ArithmeticOperation, Toggle)
  step _ Do = (Nothing, On)
  step _ Dont = (Nothing, Off)
  step On (Mul a b) = (Just (Multiply a b), On)
  step Off (Mul _ _) = (Nothing, Off)

feedInstructions :: Mealy Instruction (Maybe ArithmeticOperation) -> [Instruction] -> [Maybe ArithmeticOperation]
feedInstructions machine = M.run . (M.auto machine M.<~) . M.source

process :: [Instruction] -> [ArithmeticOperation]
process = catMaybes . feedInstructions toggle

-- parsing

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

parser :: Parser [Instruction]
parser = many (try (skipManyTill anySingle (try operationParser)))

operationParser :: Parser Instruction
operationParser =
  (Dont <$ string "don't()")
    <|> (Do <$ string "do()")
    <|> (Mul <$> (string "mul(" *> decimal <* char ',') <*> (decimal <* char ')'))

parseAll :: T.Text -> Either ParsingError [Instruction]
parseAll = runParser parser "input file"
