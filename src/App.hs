module App where

import Args 
import Options.Applicative ( handleParseResult )
import System.Environment (getArgs)
import Day1 ( program )
import Day2 ( program )
import Day3 ( program )
import Day4 ( program )
import Day5 ( program )
import Day6 ( program )
import Day7 ( program )
import Day8 ( program )
import Day9 ( program )
import Day10 ( program )

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Args -> IO ()
program' (Args 1 f) = Day1.program f
program' (Args 2 f) = Day2.program f
program' (Args 3 f) = Day3.program f
program' (Args 4 f) = Day4.program f
program' (Args 5 f) = Day5.program f
program' (Args 6 f) = Day6.program f
program' (Args 7 f) = Day7.program f
program' (Args 8 f) = Day8.program f
program' (Args 9 f) = Day9.program f
program' (Args 10 f) = Day10.program f
program' _ = putStrLn "day not found"
