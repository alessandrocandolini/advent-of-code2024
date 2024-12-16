{-# LANGUAGE QuasiQuotes #-}

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
  , [Obstruction, OpenSpace, OpenSpace, Guard North, OpenSpace, Obstruction]
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
    , ((3, 2), Guard North)
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

parseLoadAndWalk :: T.Text -> Either Error [(Position, Maybe Direction)]
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
    loadAndWalk simple `shouldBe` Right [((3, 2), Just North), ((3, 1), Just North), ((4, 1), Just East), ((4, 2), Just South), ((4, 3), Just South), ((3, 3), Just West), ((2, 3), Just West), ((2, 2), Just North), ((2, 1), Just North), ((2, 0), Just North)]

  it "walk never hitting the edges" $
    parseLoadAndWalk stuck `shouldBe` Right [((3, 1), Just North)]

  it "walk" $
    parseLoadAndWalk input
      `shouldBe` Right
        [ ((4, 6), Just North)
        , ((4, 5), Just North)
        , ((4, 4), Just North)
        , ((4, 3), Just North)
        , ((4, 2), Just North)
        , ((4, 1), Just North)
        , ((5, 1), Just East)
        , ((6, 1), Just East)
        , ((7, 1), Just East)
        , ((8, 1), Just East)
        , ((8, 2), Just South)
        , ((8, 3), Just South)
        , ((8, 4), Just South)
        , ((8, 5), Just South)
        , ((8, 6), Just South)
        , ((7, 6), Just West)
        , ((6, 6), Just West)
        , ((5, 6), Just West)
        , ((4, 6), Just West)
        , ((3, 6), Just West)
        , ((2, 6), Just West)
        , ((2, 5), Just North)
        , ((2, 4), Just North)
        , ((3, 4), Just East)
        , ((4, 4), Just East)
        , ((5, 4), Just East)
        , ((6, 4), Just East)
        , ((6, 5), Just South)
        , ((6, 6), Just South)
        , ((6, 7), Just South)
        , ((6, 8), Just South)
        , ((5, 8), Just West)
        , ((4, 8), Just West)
        , ((3, 8), Just West)
        , ((2, 8), Just West)
        , ((1, 8), Just West)
        , ((1, 7), Just North)
        , ((2, 7), Just East)
        , ((3, 7), Just East)
        , ((4, 7), Just East)
        , ((5, 7), Just East)
        , ((6, 7), Just East)
        , ((7, 7), Just East)
        , ((7, 8), Just South)
        , ((7, 9), Just South)
        ]

  it "logic" $
    logic input `shouldBe` Right (Answer 41)
