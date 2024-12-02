module Main where

import Criterion.Main
import Day1Bench
import Day2Bench

main :: IO ()
main = defaultMain (Day1Bench.benchmarks ++ Day2Bench.benchmarks)
