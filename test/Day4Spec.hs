module Day4Spec where

import Day4
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Text.IO as T

spec :: Spec
spec = describe "Day 4" $ do
  it ""
    $ 1
    `shouldBe` 1

  prop ""
    $ \l -> reverse (reverse l) == (l :: [Int])

  xit "solve the puzzle" $ do
     input <- T.readFile "resources/input4"
     logic input `shouldBe` Answer