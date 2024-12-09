module Day6 where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

class ToChar a where
  toChar :: a -> Char

class FromChar a where
  fromChar :: Char -> Maybe a

fromCharEnum :: (Enum a, Bounded a, ToChar a) => Char -> Maybe a
fromCharEnum = flip M.lookup m
 where
  m = M.fromList (fmap (\p -> (toChar p, p)) [minBound .. maxBound])

data Direction = Leftward | Rightward | Up | Down deriving (Eq, Show, Enum, Bounded)

instance ToChar Direction where
  toChar Leftward = '<'
  toChar Rightward = '>'
  toChar Up = '^'
  toChar Down = 'v'

instance FromChar Direction where
  fromChar = fromCharEnum

data Location = Available | Obstacle | Head Direction deriving (Eq, Show)

instance ToChar Location where
  toChar Available = '.'
  toChar Obstacle = '#'
  toChar (Head d) = toChar d

instance FromChar Location where
  fromChar '.' = Just Available
  fromChar '#' = Just Obstacle
  fromChar c = Head <$> fromChar c
data Answer = Answer deriving (Eq, Show)

logic :: T.Text -> Answer
logic = const Answer

