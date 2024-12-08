{-# LANGUAGE QuasiQuotes #-}

module Day3Spec where

import qualified Data.Text as T
import Day3
import SpecUtils (shouldBePretty)
import Test.Hspec (Spec, describe, it, shouldBe)

example1 :: T.Text
example1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

instructions1 :: [Instruction]
instructions1 = [Mul 2 4, Mul 5 5, Mul 11 8, Mul 8 5]

example2 :: T.Text
example2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

instructions2 :: [Instruction]
instructions2 = [Mul 2 4, Dont, Mul 5 5, Mul 11 8, Do, Mul 8 5]

spec :: Spec
spec = describe "Day 3" $ do
  it "can parse single operation" $
    parseAll "mul(2,4)" `shouldBePretty` Right [Mul 2 4]

  it "can parse single operation prepanded by text" $
    parseAll "amul(2,4)" `shouldBePretty` Right [Mul 2 4]

  it "can parse single operation prepanded and appended by text" $
    parseAll "amul(2,4)a" `shouldBePretty` Right [Mul 2 4]

  it "can parse multiple operations" $
    parseAll "mul(2,4)mul(3,5)" `shouldBePretty` Right [Mul 2 4, Mul 3 5]

  it "can parse multiple operations prepanded by text" $
    parseAll "amul(2,4)mul(3,5)" `shouldBePretty` Right [Mul 2 4, Mul 3 5]

  it "can parse multiple operations prepanded and separated by text" $
    parseAll "amul(2,4)amul(3,5)" `shouldBePretty` Right [Mul 2 4, Mul 3 5]

  it "can parse multiple operations separated by text that looks like an operation almost (ie, can backtrack)" $
    parseAll "amul(2,4)amul(10,amul(3,5)" `shouldBePretty` Right [Mul 2 4, Mul 3 5]

  it "can parse multiple operations separated by text and ending with extra" $
    parseAll "amul(2,4)amul(3,5)a" `shouldBePretty` Right [Mul 2 4, Mul 3 5]

  it "can parse mul(decimal,decimal) in part1" $
    parseAll example1 `shouldBePretty` Right instructions1

  it "can parse mul(decimal,decimal) in part2" $
    parseAll example2 `shouldBePretty` Right instructions2

  it "mapToggle hydrates the status of the toggle on each input" $
    mapToggle fromInstruction instructions2
      `shouldBe` [ (On, Multiply 2 4)
                 , (Off, Multiply 5 5)
                 , (Off, Multiply 11 8)
                 , (On, Multiply 8 5)
                 ]

  it "answer part 1" $
    logic example1 `shouldBePretty` Right (Answer 161 161)
  it "answer part 2" $
    logic example2 `shouldBePretty` Right (Answer 161 48)
