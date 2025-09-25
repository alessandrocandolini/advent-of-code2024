module App where

import Args (
  Args (Args),
  Command (..),
  GenerateArgs (GenerateArgs),
  StatsArgs (StatsArgs),
  parseArgs,
  readInput,
 )
import CodeGenerator (program)
import qualified Data.Text as T
import Options.Applicative (handleParseResult)
import Solutions (solutions)
import Stats (program)
import System.Environment (getArgs)

program :: IO ()
program =
  getArgs >>= (handleParseResult . parseArgs) >>= program'

program' :: Command -> IO ()
program' (Run (Args n input)) = readInput input >>= runSolution n
program' (Generate (GenerateArgs d)) = CodeGenerator.program d
program' (GetStats (StatsArgs year export)) = Stats.program year export

runSolution :: Int -> T.Text -> IO ()
runSolution day input = case solution day of
  Just solve -> solve input
  Nothing -> putStrLn "day not found"

solution :: Int -> Maybe (T.Text -> IO ())
solution day
  | day >= 1 && day <= length solutions = Just (solutions !! (day - 1))
  | otherwise = Nothing
