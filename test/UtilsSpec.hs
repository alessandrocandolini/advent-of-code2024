{-# LANGUAGE OverloadedLists #-}

module UtilsSpec where

import Data.Map as M
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Utils (groupMap)

spec :: Spec
spec = describe "utils" $ do
  it "groupMap" $
    let
      input :: [Int]
      input = [1, 2, 3, 4, 5, 6]
      expected :: M.Map Bool [Int]
      expected = [(True, [20, 40, 60]), (False, [10, 30, 50])]
     in
      groupMap even (10 *) input `shouldBe` expected
