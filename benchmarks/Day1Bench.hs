module Day1Bench (benchmarks) where

import Criterion.Main
import Day1 (part1, part2)

generateInput :: Int -> [(Int, Int)]
generateInput n = zip [1 .. n] (reverse [1 .. n])

benchmarks :: [Benchmark]
benchmarks =
  [ bgroup
      "part1"
      (fmap (\n -> bench (show n) $ whnf part1 (generateInput n)) [1000, 10000, 100000])
  , bgroup
      "part2"
      (fmap (\n -> bench (show n) $ whnf part2 (generateInput n)) [1000, 10000, 100000])
  ]
