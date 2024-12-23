module Day3 where

import Control.Applicative (Alternative ((<|>)), many)
import Data.Machine (Mealy)
import qualified Data.Machine as M
import Data.Semigroup (Sum (..))
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, runParser, skipManyTill)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Witherable (catMaybes, mapMaybe)

program :: T.Text -> IO ()
program = print . logic

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
part1 = evalAll . mapMaybe ignoreDoDontInstructions
 where
  ignoreDoDontInstructions (Mul a b) = Just (Multiply a b)
  ignoreDoDontInstructions _ = Nothing

part2 :: [Instruction] -> Int
part2 = evalAll . processInstructions

data Toggle = On | Off deriving (Eq, Show)

toggle :: (a -> Either Toggle b) -> Mealy a (Maybe (Toggle, b))
toggle p = M.unfoldMealy (step p) On
 where
  step :: (a -> Either Toggle b) -> Toggle -> a -> (Maybe (Toggle, b), Toggle)
  step q t a = case (t, q a) of
    (_, Left t') -> (Nothing, t')
    (On, Right b) -> (Just (On, b), On)
    (Off, Right b) -> (Just (Off, b), Off)

mapToggle :: (a -> Either Toggle b) -> [a] -> [(Toggle, b)]
mapToggle p = catMaybes . M.run . (M.auto machine M.<~) . M.source
 where
  machine = toggle p

filterByToggle :: (a -> Either Toggle b) -> ((Toggle, b) -> Bool) -> [a] -> [b]
filterByToggle p q = map snd . filter q . mapToggle p

processInstructions :: [Instruction] -> [ArithmeticOperation]
processInstructions = filterByToggle fromInstruction isOn
 where
  isOn = (==) On . fst

fromInstruction :: Instruction -> Either Toggle ArithmeticOperation
fromInstruction Do = Left On
fromInstruction Dont = Left Off
fromInstruction (Mul a b) = Right (Multiply a b)

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
