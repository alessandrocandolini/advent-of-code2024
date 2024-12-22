{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedLists #-}

module Day6Spec where

import Data.Array (Array, array)
import Data.Bifunctor (first)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Day6
import NeatInterpolation (trimming)
import SpecUtils (shouldBePretty)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck
import Test.QuickCheck

input :: T.Text
input =
  [trimming|
....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
|]

stuck :: T.Text
stuck =
  [trimming|
...#..
#.#^##
...##.
|]

simpleInput :: T.Text
simpleInput =
  [trimming|
...#..
.....#
#..^.#
.#....
....#.
|]

simple :: [[Cell]]
simple =
  [ [OpenSpace, OpenSpace, OpenSpace, Obstruction, OpenSpace, OpenSpace]
  , [OpenSpace, OpenSpace, OpenSpace, OpenSpace, OpenSpace, Obstruction]
  , [Obstruction, OpenSpace, OpenSpace, Patrol North, OpenSpace, Obstruction]
  , [OpenSpace, Obstruction, OpenSpace, OpenSpace, OpenSpace, OpenSpace]
  , [OpenSpace, OpenSpace, OpenSpace, OpenSpace, Obstruction, OpenSpace]
  ]

simpleArray :: Array Position Cell
simpleArray =
  array
    ((0, 0), (5, 4))
    [ ((0, 0), OpenSpace)
    , ((0, 1), OpenSpace)
    , ((0, 2), Obstruction)
    , ((0, 3), OpenSpace)
    , ((0, 4), OpenSpace)
    , ((1, 0), OpenSpace)
    , ((1, 1), OpenSpace)
    , ((1, 2), OpenSpace)
    , ((1, 3), Obstruction)
    , ((1, 4), OpenSpace)
    , ((2, 0), OpenSpace)
    , ((2, 1), OpenSpace)
    , ((2, 2), OpenSpace)
    , ((2, 3), OpenSpace)
    , ((2, 4), OpenSpace)
    , ((3, 0), Obstruction)
    , ((3, 1), OpenSpace)
    , ((3, 2), Patrol North)
    , ((3, 3), OpenSpace)
    , ((3, 4), OpenSpace)
    , ((4, 0), OpenSpace)
    , ((4, 1), OpenSpace)
    , ((4, 2), OpenSpace)
    , ((4, 3), OpenSpace)
    , ((4, 4), Obstruction)
    , ((5, 0), OpenSpace)
    , ((5, 1), Obstruction)
    , ((5, 2), Obstruction)
    , ((5, 3), OpenSpace)
    , ((5, 4), OpenSpace)
    ]

parseLoadAndWalk :: T.Text -> Either Error [(Position, Direction)]
parseLoadAndWalk text = (=<<) loadAndWalk (first ParsingError (parse text))

spec :: Spec
spec = describe "Day 6" $ do
  it "all directions clockwise" $
    directionsClockwise South `shouldBe` [South, West, North, East]

  it "parse successfully a simple input" $
    parse simpleInput `shouldBePretty` Right simple

  it "buildArray" $ do
    listToArray simple `shouldBe` simpleArray -- array ((0, 0), (0, 0)) [((0, 0), OpenSpace)]
  it "walk simple example" $
    loadAndWalk simple `shouldBe` Right [((3, 2), North), ((3, 1), North), ((4, 1), East), ((4, 2), South), ((4, 3), South), ((3, 3), West), ((2, 3), West), ((2, 2), North), ((2, 1), North), ((2, 0), North)]

  it "walk never hitting the edges" $
    parseLoadAndWalk stuck `shouldBe` Right [((3, 1), North)]

  it "walk" $
    parseLoadAndWalk input
      `shouldBe` Right
        [ ((4, 6), North)
        , ((4, 5), North)
        , ((4, 4), North)
        , ((4, 3), North)
        , ((4, 2), North)
        , ((4, 1), North)
        , ((5, 1), East)
        , ((6, 1), East)
        , ((7, 1), East)
        , ((8, 1), East)
        , ((8, 2), South)
        , ((8, 3), South)
        , ((8, 4), South)
        , ((8, 5), South)
        , ((8, 6), South)
        , ((7, 6), West)
        , ((6, 6), West)
        , ((5, 6), West)
        , ((4, 6), West)
        , ((3, 6), West)
        , ((2, 6), West)
        , ((2, 5), North)
        , ((2, 4), North)
        , ((3, 4), East)
        , ((4, 4), East)
        , ((5, 4), East)
        , ((6, 4), East)
        , ((6, 5), South)
        , ((6, 6), South)
        , ((6, 7), South)
        , ((6, 8), South)
        , ((5, 8), West)
        , ((4, 8), West)
        , ((3, 8), West)
        , ((2, 8), West)
        , ((1, 8), West)
        , ((1, 7), North)
        , ((2, 7), East)
        , ((3, 7), East)
        , ((4, 7), East)
        , ((5, 7), East)
        , ((6, 7), East)
        , ((7, 7), East)
        , ((7, 8), South)
        , ((7, 9), South)
        ]

  it "logic" $
    logic input `shouldBe` Right (Answer 41)
