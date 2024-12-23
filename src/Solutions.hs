module Solutions where

import qualified Data.Text as T
import Day1 (program)
import Day2 (program)
import Day3 (program)
import Day4 (program)
import Day5 (program)
import Day6 (program)

solutions :: [T.Text -> IO ()]
solutions =
  [ Day1.program
  , Day2.program
  , Day3.program
  , Day4.program
  , Day5.program
  , Day6.program
  ]
