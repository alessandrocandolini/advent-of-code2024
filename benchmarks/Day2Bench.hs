{-# LANGUAGE NumericUnderscores #-}

module Day2Bench (benchmarks) where

import Criterion.Main
import Day2 (part1, part2, Report(..), Level(..))

generateInput :: Int -> Int -> [Report]
generateInput numReports numLevels =
  [ Report [Level(i + j) | j <- [1 .. numLevels]] | i <- [1 .. numReports] ]

values :: [(Int, Int)]
values = [(100, 1000), (1000, 1000), (5000, 1000)]

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "Day 2 part 1"
      (fmap (\a -> bench (show a) $ whnf part1 (uncurry generateInput a)) values)
  , bgroup
      "Day 2 part 2"
      (fmap (\a -> bench (show a) $ whnf part2 (uncurry generateInput a)) values)
  ]
