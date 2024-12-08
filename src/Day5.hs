{-# LANGUAGE DerivingVia #-}

module Day5 where

import Algebra.Graph (edges)
import Algebra.Graph.AdjacencyMap.Algorithm (Cycle)
import Algebra.Graph.ToGraph (topSort)
import Control.Monad (mfilter)
import Data.Bifunctor (Bifunctor (first))
import Data.List (intercalate, sortBy)
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Semigroup (Sum (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Safe (headMay)
import System.Console.ANSI (Color (..))
import Text.Megaparsec (Parsec, endBy, runParser, sepBy)
import Text.Megaparsec.Char (char, newline)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)
import Witherable (catMaybes, mapMaybe)
import Utils (colorText)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data GraphResolutionError a = GraphResolutionError (Cycle a) [(a, a)] [a] deriving (Eq)

explicitCycle :: Cycle a -> [(a, a)]
explicitCycle c = zip (N.toList c) (N.tail c ++ [N.head c])

informativeMessage :: (Eq a, Show a) => GraphResolutionError a -> String
informativeMessage (GraphResolutionError c rules as) =
  let
    c' = explicitCycle c
    highlightRule rule
      | elem rule c' = colorText Yellow (show rule)
      | otherwise = show rule
    highlightRules = intercalate ", " (fmap highlightRule rules)
   in
    intercalate
      "\n"
      [ "Circular dependency found: " ++ show c'
      , "Input: " ++ show as
      , "Rules: " ++ highlightRules
      ]

instance (Eq a, Show a) => Show (GraphResolutionError a) where
  show = informativeMessage

data Error
  = EvaluationError (GraphResolutionError Page)
  | ParsingInputError ParsingError
  deriving (Eq, Show)

data Answer = Answer Int Int deriving (Eq, Show)

type Input = ([Rule], [[Page]])

answer :: Input -> Either (GraphResolutionError Page) Answer
answer input = do
  p1 <- uncurry part1 input
  p2 <- uncurry part2 input
  pure (Answer p1 p2)

logic :: T.Text -> Either Error Answer
logic text = do
  input <- first ParsingInputError (parse text)
  first EvaluationError (answer input)

newtype Page = Page Int
  deriving (Eq, Show, Num, Ord) via Int

data Rule = Rule Page Page deriving (Eq, Show)

toPair :: Rule -> (Page, Page)
toPair (Rule p1 p2) = (p1, p2)

pageScore :: Page -> Sum Int
pageScore (Page p) = Sum p

calculateOrderingFromTopological :: (Ord a) => [a] -> (a -> a -> Ordering)
calculateOrderingFromTopological topological = p (index topological)
 where
  index :: (Ord b) => [b] -> Map b Int
  index input = M.fromList (zip input [0 ..])
  p m a b = case (M.lookup a m, M.lookup b m) of
    (Just px, Just py) -> compare px py
    _ -> EQ

calculateOrdering :: (Ord a) => [(a, a)] -> Either (Cycle a) (a -> a -> Ordering)
calculateOrdering rules = fmap calculateOrderingFromTopological (topSort (edges rules))

sumMiddles :: (Num b) => (a -> Sum b) -> [[a]] -> b
sumMiddles p =
  getSum
    . foldMap p
    . mapMaybe middle

middle :: [a] -> Maybe a
middle as = headMay (drop half as)
 where
  half = div (length as) 2

part1 :: [Rule] -> [[Page]] -> Either (GraphResolutionError Page) Int
part1 rules = fmap (sumMiddles pageScore . catMaybes) . traverse (isSorted rules')
 where
  rules' = fmap toPair rules

part2 :: [Rule] -> [[Page]] -> Either (GraphResolutionError Page) Int
part2 rules = fmap (sumMiddles pageScore . catMaybes) . traverse (isNotSorted rules')
 where
  rules' = fmap toPair rules

isSorted :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) (Maybe [a])
isSorted rules as = fmap (mfilter (as ==) . Just) (sortByRules rules as)

isNotSorted :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) (Maybe [a])
isNotSorted rules as = fmap (mfilter (as /=) . Just) (sortByRules rules as)

sortByRules :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) [a]
sortByRules rules as = do
  ordering <- calculateOrderingFilteringRules rules as
  pure (sortBy ordering as)

calculateOrderingFilteringRules :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) (a -> a -> Ordering)
calculateOrderingFilteringRules rules as = first hydrateError (calculateOrdering rules')
 where
  rules' = applicableRules as rules
  hydrateError e = GraphResolutionError e rules' as

-- keep only the subset of rules relevant for the current input
applicableRules :: (Ord a) => [a] -> [(a, a)] -> [(a, a)]
applicableRules as = filter (isApplicable s)
 where
  s = S.fromList as

isApplicable :: (Ord a) => S.Set a -> (a, a) -> Bool
isApplicable s (a1, a2) = S.member a1 s && S.member a2 s

-- parsing

type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

pageParser :: Parser Page
pageParser = fmap Page decimal

ruleParser :: Parser Rule
ruleParser = Rule <$> (pageParser <* char '|') <*> pageParser

rulesParser :: Parser [Rule]
rulesParser = endBy ruleParser newline

pagesParser :: Parser [Page]
pagesParser = sepBy pageParser (char ',')

updatesParser :: Parser [[Page]]
updatesParser = sepBy pagesParser newline

parser :: Parser Input
parser =
  (,)
    <$> (rulesParser <* newline)
    <*> updatesParser

parse :: T.Text -> Either ParsingError Input
parse = runParser parser "input"
