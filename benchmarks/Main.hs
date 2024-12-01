module Main where

import Criterion.Main
import Day1Bench

main :: IO ()
main = defaultMain (Day1Bench.benchmarks)
