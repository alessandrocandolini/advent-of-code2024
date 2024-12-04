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

slices :: Grid a -> [[a]]
slices g = horizontalSlices g ++ verticalSlices g ++ diagonalSlices g

findXmasOccurrences :: Grid Char -> Occurrences Xmas
findXmasOccurrences = findOccurrences parse

spec :: Spec
spec = describe "Day 4" $ do
  it "slices on a single row" $
    let
      input :: String
      input = "MMMSXXMASM"
     in
      slices (Grid [input]) `shouldBe` ["MMMSXXMASM", "MMSXXMASM", "MSXXMASM", "SXXMASM", "XXMASM", "XMASM", "MASM", "ASM", "SM", "M", "", "M", "", "M", "", "M", "", "S", "", "X", "", "X", "", "M", "", "A", "", "S", "", "M", "", ""]

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
      (horizontalSlices input ++ verticalSlices input) `shouldBe` expected

  it "parse xmas" $
    parse "XMASM" `shouldBe` Just Xmas

  it "findOccurrences in one line" $
    findXmasOccurrences (Grid ["MMMSXXMASM"]) `shouldBe` (Occurrences [Xmas] [] [])

  it "findOccurrences in puzzle example" $
    findXmasOccurrences (parseGrid example)
      `shouldBe` Occurrences
        { horizontal = [Xmas, Samx, Xmas, Samx, Xmas]
        , vertical = [Samx, Xmas, Samx]
        , diagonal = [Samx, Xmas, Samx, Samx, Samx, Samx, Xmas, Xmas, Xmas, Xmas]
        }

  it "part1" $
    (part1 . parseGrid) example `shouldBe` 18
