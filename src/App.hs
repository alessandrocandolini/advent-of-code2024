module App where

import Args
import Day1 (program)
import Day10 (program)
import Day2 (program)
import Day3 (program)
import Day4 (program)
import Day5 (program)
import Day6 (program)
import Day7 (program)
import Day8 (program)
import Day9 (program)
import Options.Applicative (handleParseResult)
import System.Environment (getArgs)

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Command -> IO ()
program' (Run (Args 1 f)) = Day1.program f
program' (Run (Args 2 f)) = Day2.program f
program' (Run (Args 3 f)) = Day3.program f
program' (Run (Args 4 f)) = Day4.program f
program' (Run (Args 5 f)) = Day5.program f
program' (Run (Args 6 f)) = Day6.program f
program' (Run (Args 7 f)) = Day7.program f
program' (Run (Args 8 f)) = Day8.program f
program' (Run (Args 9 f)) = Day9.program f
program' (Run (Args 10 f)) = Day10.program f
program' (Run (Args _ _)) = putStrLn "day not found"
program' (Generate (GenerateArgs _)) = putStrLn "not implemented yet"
