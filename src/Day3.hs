module Day3 where

import Control.Applicative (Alternative ((<|>)), many)
import Data.Bifunctor (Bifunctor (bimap))
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
part2 = evalAll . interpreter

data Toggle = On | Off deriving (Eq, Show)

data Memory = Memory
  { toggle :: Toggle
  , operations :: [ArithmeticOperation]
  }
  deriving (Eq, Show)

data MemoryInstruction = SwitchToggle Toggle | AppendIfOn ArithmeticOperation deriving (Eq, Show)

fromInstruction :: Instruction -> MemoryInstruction
fromInstruction (Mul a b) = AppendIfOn (Multiply a b)
fromInstruction Dont = SwitchToggle Off
fromInstruction Do = SwitchToggle On

empty :: Memory
empty = Memory On []

run :: Memory -> MemoryInstruction -> Memory
run (Memory _ ops) (SwitchToggle t) = Memory t ops
run (Memory On ops) (AppendIfOn o) = Memory On (o : ops)
run m@(Memory Off _) (AppendIfOn _) = m

runAll :: Memory -> [MemoryInstruction] -> Memory
runAll = foldl' run

interpreter :: [Instruction] -> [ArithmeticOperation]
interpreter = reverse . operations . runAll empty . fmap fromInstruction

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
