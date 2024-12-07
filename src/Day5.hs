{-# LANGUAGE DerivingVia #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}

module Day5 where

import Algebra.Graph (edges)
import Algebra.Graph.AdjacencyMap.Algorithm (Cycle)
import qualified Algebra.Graph.AdjacencyMap.Algorithm
import Algebra.Graph.ToGraph (topSort)
import Control.Applicative (many)
import Control.Monad (mfilter)
import Data.Bifunctor (Bifunctor (first))
import Data.List (intercalate, sortBy, unfoldr)
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (listToMaybe)
import Data.Semigroup (Sum (..))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Void (Void)
import Safe (headMay)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, endBy, optional, runParser, sepBy, skipManyTill)
import Text.Megaparsec.Char (char, newline, space, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle, errorBundlePretty)
import Witherable (catMaybes, mapMaybe)

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer Int Int deriving (Eq, Show)

data GraphResolutionError a = GraphResolutionError (Cycle a) [(a, a)] [a] deriving (Eq)
data Error = EvaluationError (GraphResolutionError Page) | ParsingInputError ParsingError deriving (Eq, Show)

fullCycle :: Cycle a -> [(a, a)]
fullCycle c = zip (N.toList c) (N.tail c ++ [N.head c])

instance (Show a) => Show (GraphResolutionError a) where
  show (GraphResolutionError c rules pages) =
    intercalate
      "\n\n"
      [ "Cyclic dependency detected: " ++ show c
      , "while analysing the following list of pages:"
      , show pages
      , "Cycle:"
      , show (fullCycle c)
      , "Rules applied:"
      , show rules
      ]

type Input = ([Rule], [[Page]])

answer :: Input -> Either (GraphResolutionError Page) Answer
answer input = do
  p1 <- uncurry part1 input
  p2 <- uncurry part2 input
  pure (Answer p1 p2)

logic :: T.Text -> Either Error Answer
logic =
  (=<<) (first EvaluationError . answer)
    . ( first ParsingInputError
          . parse
      )

data Rule = Rule Page Page deriving (Eq, Show)

newtype Page = Page Int
  deriving (Eq, Show, Num, Ord) via Int

toPair :: Rule -> (Page, Page)
toPair (Rule p1 p2) = (p1, p2)

pageScore :: Page -> Sum Int
pageScore (Page p) = Sum p

fromTopological :: (Ord a) => [a] -> (a -> a -> Ordering)
fromTopological topological = p (index topological)
 where
  index :: (Ord b) => [b] -> Map b Int
  index input = M.fromList (zip input [0 ..])
  p m a b = case (M.lookup a m, M.lookup b m) of
    (Just px, Just py) -> compare px py
    _ -> EQ

calculateOrdering :: (Ord a) => [(a, a)] -> Either (Cycle a) (a -> a -> Ordering)
calculateOrdering rules = fmap fromTopological (topSort (edges rules))

sumMiddle :: (Num b) => (a -> Sum b) -> [[a]] -> b
sumMiddle p =
  getSum
    . foldMap p
    . mapMaybe middle

middle :: [a] -> Maybe a
middle as = headMay (drop half as)
 where
  half = div (length as) 2

part1 :: [Rule] -> [[Page]] -> Either (GraphResolutionError Page) Int
part1 rules = fmap (sumMiddle pageScore . catMaybes) . traverse (isSorted rules')
 where
  rules' = fmap toPair rules

part2 :: [Rule] -> [[Page]] -> Either (GraphResolutionError Page) Int
part2 rules = fmap (sumMiddle pageScore . catMaybes) . traverse (isNotSorted rules')
 where
  rules' = fmap toPair rules

isSorted :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) (Maybe [a])
isSorted rules as = fmap (mfilter (as ==) . Just) (sortByRules rules as)

isNotSorted :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) (Maybe [a])
isNotSorted rules as = fmap (mfilter (as /=) . Just) (sortByRules rules as)

sortByRules :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) [a]
sortByRules rules as = fmap (`sortBy` as) (calculateOrderingByApplicableRules rules as)

calculateOrderingByApplicableRules :: (Ord a) => [(a, a)] -> [a] -> Either (GraphResolutionError a) (a -> a -> Ordering)
calculateOrderingByApplicableRules rules as = ordering
 where
  rules' = applicableRules as rules
  ordering = first hydrateError (calculateOrdering rules')
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
