module Args where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Options.Applicative

data Command
  = Run Args
  | Generate GenerateArgs
  | GetStats StatsArgs
  deriving (Eq, Show)

data Args = Args
  { day :: Int
  , input :: DayInput
  }
  deriving (Eq, Show)

data DayInput = FromFile FilePath | FromStdInput deriving (Eq, Show)

readInput :: DayInput -> IO T.Text
readInput (FromFile f) = T.readFile f
readInput FromStdInput = T.getContents

dayInputParser :: Parser DayInput
dayInputParser = fromFileParser <|> fromStdInputParser
 where
  fromFileParser =
    FromFile
      <$> strOption
        ( long "filename"
            <> short 'f'
            <> help "Read input from the specified file"
        )
  fromStdInputParser =
    flag'
      FromStdInput
      ( long "with-input"
          <> help "Read input from standard input"
      )

newtype GenerateArgs = GenerateArgs Int deriving (Eq, Show)
data StatsRender = ConsoleRender | JsonRender deriving (Eq, Show)
data StatsArgs = StatsArgs Int StatsRender deriving (Eq, Show)

argsParser :: Parser Args
argsParser =
  Args
    <$> option
      auto
      ( long "day"
          <> short 'd'
          <> help "day"
      )
    <*> dayInputParser

generateArgsParser :: Parser GenerateArgs
generateArgsParser =
  GenerateArgs
    <$> option
      auto
      ( long "day"
          <> short 'd'
          <> help "day"
      )
statsArgsParser :: Parser StatsArgs
statsArgsParser =
  StatsArgs
    <$> option
      auto
      ( long "year"
          <> short 'y'
          <> value 2024
          <> showDefault
          <> help "year"
      )
    <*> flag
      ConsoleRender
      JsonRender
      ( long "json"
          <> help "Export as json"
      )

commandParser :: Parser Command
commandParser =
  hsubparser
    ( command "run" (info (Run <$> argsParser) (progDesc "run the solution to the puzzle"))
        <> command "generate" (info (Generate <$> generateArgsParser) (progDesc "generate scaffolding from template for a given day"))
        <> command "stats" (info (GetStats <$> statsArgsParser) (progDesc "retrieve stats from the AOC website"))
    )

withInfo :: Parser a -> String -> ParserInfo a
withInfo p s = info (helper <*> p) (fullDesc <> progDesc s)

parseArgs :: [String] -> ParserResult Command
parseArgs = execParserPure preferences parserInfo
 where
  parserInfo = withInfo commandParser "Advent of code 2024 CLI"
  preferences = prefs (disambiguate <> showHelpOnEmpty <> showHelpOnError)
