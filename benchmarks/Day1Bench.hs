{-# LANGUAGE NumericUnderscores #-}

module Day1Bench (benchmarks) where

import Criterion.Main
import Day1 (part1, part2)

generateInput :: Int -> [(Int, Int)]
generateInput n = zip [1 .. n] (reverse [1 .. n])

values :: [Int]
values = [1000, 10_000, 100_000]

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "Day 1 part 1"
      (fmap (\n -> bench (show n) $ whnf part1 (generateInput n)) values)
  , bgroup
      "Day 1 part 2"
      (fmap (\n -> bench (show n) $ whnf part2 (generateInput n)) values)
  ]
