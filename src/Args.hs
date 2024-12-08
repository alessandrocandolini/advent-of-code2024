module Args where

import Options.Applicative

data Command
  = Run Args
  | Generate GenerateArgs
  | GetStats StatsArgs
  deriving (Eq, Show)

data Args = Args
  { day :: Int
  , input :: FilePath
  }
  deriving (Eq, Show)

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
    <*> strOption
      ( long "filename"
          <> short 'f'
          <> help "filename"
      )

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
