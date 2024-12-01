{-# LANGUAGE QuasiQuotes #-}

module Day1Spec where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day1 (parse, part1, part2)
import NeatInterpolation (trimming)
import SpecUtils (shouldBePretty)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
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
    parse "3 4" `shouldBePretty` Right (3, 4)

  it "part 1" $
    (fmap part1 . traverse parse . T.lines) example `shouldBePretty` Right 11

  it "part 2" $
    (fmap part2 . traverse parse . T.lines) example `shouldBePretty` Right 31
