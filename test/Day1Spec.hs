{-# LANGUAGE QuasiQuotes #-}

module Day1Spec where

import qualified Data.Text as T
import Day1 (logic, parseAll, part1, part2, Answer(..))
import NeatInterpolation (trimming)
import SpecUtils (shouldBePretty)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.QuickCheck ()
import Test.QuickCheck ()

example :: T.Text
example =
  [trimming|
      3   4
      4   3
      2   5
      1   3
      3   9
      3   3
      |]

spec :: Spec
spec = describe "Day 1" $ do
  it "parse line" $
    parseAll "3 4" `shouldBePretty` Right [(3, 4)]

  it "part 1" $
    (fmap part1 . parseAll) example `shouldBePretty` Right 11

  it "part 2" $
    (fmap part2 . parseAll) example `shouldBePretty` Right 31

  it "both" $
    logic example `shouldBePretty` Right (Answer 11 31)
