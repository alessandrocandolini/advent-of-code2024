{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE QuasiQuotes #-}
-- for IsList
{-# LANGUAGE TypeFamilies #-}

module Day4Spec where

import Data.List (tails)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day4
import GHC.Exts (IsList (..))
import NeatInterpolation (trimming)
import SpecUtils (shouldBePretty)
import Test.Hspec (Spec, describe, it, shouldBe, xit)
import Test.Hspec.QuickCheck
import Test.QuickCheck

instance IsList (Grid a) where
  type Item (Grid a) = [a]
  fromList = Grid
  toList = grid

example :: T.Text
example =
  [trimming|
      MMMSXXMASM
      MSAMXMSMSA
      AMXSXMAAMM
      MSAMASMSMX
      XMASAMXAMM
      XXAMMXXAMA
      SMSMSASXSS
      SAXAMASAAA
      MAMMMXMMMM
      MXMXAXMASX
 |]

simple :: T.Text
simple =
  [trimming|
                    MSXA
                    MSAM
                    AMXS
                  |]

matrix :: Grid Int
matrix =
  [ [1, 2, 3]
  , [4, 5, 6]
  , [7, 8, 9]
  ]

findXmasOccurrences :: Grid Char -> Occurrences Xmas
findXmasOccurrences = findOccurrences parse

spec :: Spec
spec = describe "Day 4" $ do
  it "parse xmas" $
    parse "XMASM" `shouldBe` Just Xmas

  it "findOccurrences in one line" $
    findXmasOccurrences ["MMMSXXMASM"] `shouldBe` Occurrences [Xmas] [] [] []

  it "rows (numeric example)" $
    rows matrix
      `shouldBe` [ [1, 2, 3]
                 , [4, 5, 6]
                 , [7, 8, 9]
                 ]

  it "columns (numeric example)" $
    columns matrix
      `shouldBe` [ [1, 4, 7]
                 , [2, 5, 8]
                 , [3, 6, 9]
                 ]

  it "diagonals (numeric example)" $
    diagonals matrix
      `shouldBe` [ [1, 5, 9]
                 , [2, 6]
                 , [3]
                 , [4, 8]
                 , [5, 9]
                 , [6]
                 , [7]
                 , [8]
                 , [9]
                 ]

  it "backward diagonals (numeric example)" $
    backwardDiagonals matrix
      `shouldBe` [ [3, 5, 7]
                 , [2, 4]
                 , [1]
                 , [6, 8]
                 , [5, 7]
                 , [4]
                 , [9]
                 , [8]
                 , [7]
                 ]

  it "diagonals (text example)" $
    diagonals ((Grid . fmap T.unpack . T.lines) simple)
      `shouldBe` [ "MSX"
                 , "SAS"
                 , "XM"
                 , "A"
                 , "MM"
                 , "SX"
                 , "AS"
                 , "M"
                 , "A"
                 , "M"
                 , "X"
                 , "S"
                 ]
  -- MSXA
  -- MSAM
  -- AMXS
  it "backwad diagonals (text example)" $
    backwardDiagonals ((Grid . fmap T.unpack . T.lines) simple)
      `shouldBe` [ "AAM"
                 , "XSA"
                 , "SM"
                 , "M"
                 , "MX"
                 , "AM"
                 , "SA"
                 , "M"
                 , "S"
                 , "X"
                 , "M"
                 , "A"
                 ]

  -- it "failing test for troubleshooting" $
  -- diagonals ((Grid . fmap T.unpack . T.lines) example) `shouldBe` []
  -- it "failing test for troubleshooting 2" $
  -- fmap (\p -> (p, parse p)) (diagonals ((Grid . fmap T.unpack . T.lines) example)) `shouldBe` []

  it "findOccurrences in puzzle example" $
    findXmasOccurrences (parseGrid example)
      `shouldBe` Occurrences
        { horizontal = [Xmas, Samx, Xmas, Samx, Xmas]
        , vertical = [Samx, Xmas, Samx]
        , diagonal = [Xmas, Samx, Samx, Samx, Samx]
        , backwardDiagonal = [Samx, Xmas, Samx, Samx, Samx]
        }

  -- MSXA
  -- MSAM
  -- AMXS
  it "get squares" $
    squares3 (fmap T.unpack (T.lines simple))
      `shouldBe` [ (('M', 'M', 'A'), ('S', 'S', 'M'), ('X', 'A', 'X'))
                 , (('S', 'S', 'M'), ('X', 'A', 'X'), ('A', 'M', 'S'))
                 ]

  it "part1" $
    (part1 . parseGrid) example `shouldBe` 18

  it "part2" $
    (part2 . parseGrid) example `shouldBe` 9

  it "logic" $
    logic example `shouldBe` (Answer 18 9)
