{-# LANGUAGE QuasiQuotes #-}

module Day2Spec where

import qualified Data.Text as T
import Day2 (Answer (..), Report (..), allButOne, pairwiseDistanceAlwaysLessThan, isReportSafe1, isReportSafe2, logic, isMonotonic, parseLine, parseLines, part1, part2)
import NeatInterpolation (trimming)
import SpecUtils (shouldBePretty)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck ()
import Test.QuickCheck ()

example :: T.Text
example =
  [trimming|
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
 |]

boundPairwiseDistance = pairwiseDistanceAlwaysLessThan 3

spec :: Spec
spec = describe "Day 2" $ do
  it "isMonotonic works on empty lists" $
    isMonotonic ([] :: [Int]) `shouldBe` True

  it "isMonotonic works on singleton lists" $
    isMonotonic [1] `shouldBe` True

  it "isMonotonic increasing" $
    isMonotonic [1, 4, 7, 9, 100] `shouldBe` True

  it "isMonotonic decreasing" $
    isMonotonic [100, 9, 7, 4, 1] `shouldBe` True

  it "non isMonotonic (increasing an decreasing)" $
    isMonotonic [1, 4, 7, 6, 100] `shouldBe` False

  it "non isMonotonic (neither increasing or decreasing)" $
    isMonotonic [1, 4, 7, 7, 100] `shouldBe` False

  it "boundPairwiseDistance works on empty lists" $
    boundPairwiseDistance ([] :: [Int]) `shouldBe` True

  it "boundPairwiseDistance works on singleton lists" $
    boundPairwiseDistance [1] `shouldBe` True

  it "boundPairwiseDistance ok" $
    boundPairwiseDistance [7, 6, 4, 2, 1] `shouldBe` True

  it "boundPairwiseDistance not ok (increase of 5)" $
    boundPairwiseDistance [1, 2, 7, 8, 9] `shouldBe` False

  it "boundPairwiseDistance not ok (decrease of 4)" $
    boundPairwiseDistance [9, 7, 6, 2, 1] `shouldBe` False

  it "parse line" $
    parseLine "7 6 4 2 1" `shouldBePretty` Right (Report [7, 6, 4, 2, 1])

  it "part 1 troubleshooting" $
    fmap (fmap isReportSafe1) (parseLines example) `shouldBePretty` Right [True, False, False, False, False, True]

  it "part 1" $
    fmap part1 (parseLines example) `shouldBePretty` Right 2

  it "allButOne" $
    allButOne [1, 3, 2, 4, 5] `shouldBe` [[3, 2, 4, 5], [1, 2, 4, 5], [1, 3, 4, 5], [1, 3, 2, 5], [1, 3, 2, 4]]

  it "part 2 troubleshooting" $
    fmap (fmap isReportSafe2) (parseLines example) `shouldBePretty` Right [True, False, False, True, True, True]

  it "part 2" $
    fmap part2 (parseLines example) `shouldBePretty` Right 4

  it "logic" $
    logic example `shouldBePretty` Right (Answer 2 4)
