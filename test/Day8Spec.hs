module Day8Spec where

import Day8
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.IO as T

spec :: Spec
spec = describe "Day 5" $ do
  it ""
    $ 1
    `shouldBe` 1

  prop ""
    $ \l -> reverse (reverse l) == (l :: [Int])

  xit "solve the puzzle" $ do
     input <- T.readFile "resources/input8"
     logic input `shouldBe` Answer