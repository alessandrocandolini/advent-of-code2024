{-# LANGUAGE QuasiQuotes #-}

module Day4Spec where

import Data.List (tails)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day4
import NeatInterpolation (trimming)
import SpecUtils (shouldBePretty)
import Test.Hspec (Spec, describe, it, shouldBe, xit)
import Test.Hspec.QuickCheck
import Test.Hspec.QuickCheck ()
import Test.QuickCheck
import Test.QuickCheck ()

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

slices :: [[a]] -> [[a]]
slices g = horizontalTails g ++ verticalTails g ++ diagonalTails g

findXmasOccurrences :: Grid Char -> Occurrences Xmas
findXmasOccurrences = findOccurrences parse

spec :: Spec
spec = describe "Day 4" $ do
  it "horizontal and vertical slices" $
    let
      input :: Grid Char
      input =
        Grid
          ( lines
              ( T.unpack
                  [trimming|
                    MSXA
                    MSAM
                    AMXS
                  |]
              )
          )
      expected :: [[Char]]
      expected = ["MSXA", "SXA", "XA", "A", "", "MSAM", "SAM", "AM", "M", "", "AMXS", "MXS", "XS", "S", "", "MMA", "MA", "A", "", "SSM", "SM", "M", "", "XAX", "AX", "X", "", "AMS", "MS", "S", ""]
     in
      (horizontalTails (grid input) ++ verticalTails (grid input)) `shouldBe` expected

  it "parse xmas" $
    parse "XMASM" `shouldBe` Just Xmas

  it "findOccurrences in one line" $
    findXmasOccurrences (Grid ["MMMSXXMASM"]) `shouldBe` (Occurrences [Xmas] [] [] [])

  it "rotate diagonals to vertical columns" $
    rotateDiagonals [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[[3, 2, 1], [5, 4], [7]], [[6, 5, 4], [8, 7]], [[9, 8, 7]]]

  it "forward diagonals tails" $
    diagonalTails [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[1, 5, 9], [2, 6], [3], [4, 8], [5, 9], [6], [7], [8], [9]]

  it "rotate backward diagonals to vertical columns" $
    rotateBackwardDiagonals [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
      `shouldBe` [ [[3, 2, 1], [5, 4], [7]]
                 , [[6, 5, 4], [8, 7]]
                 , [[9, 8, 7]]
                 ]

  it "backward diagonals tails" $
    backwardDiagonalTails [[1, 2, 3], [4, 5, 6], [7, 8, 9]] `shouldBe` [[3, 5, 7], [5, 7], [7], [], [2, 4], [4], [], [1], []]

  it "findOccurrences in puzzle example" $
    findXmasOccurrences (parseGrid example)
      `shouldBe` Occurrences
        { horizontal = [Xmas, Samx, Xmas, Samx, Xmas]
        , vertical = [Samx, Xmas, Samx]
        , diagonal = [Samx, Xmas, Samx, Samx, Samx, Samx, Xmas, Xmas, Xmas, Xmas]
        , backwardDiagonal = []
        }

  it "part1" $
    (part1 . parseGrid) example `shouldBe` 18
