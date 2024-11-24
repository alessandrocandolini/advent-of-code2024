module Args where
import Options.Applicative

data Args = Args {
   day :: Int,
   input :: FilePath
   } deriving (Eq,Show)

argsParser :: Parser Args
argsParser = Args <$> option auto
            ( long "day"
           <> short 'd'
           <> help "day" ) <*> strOption ( long "filename"
           <> short 'f'
           <> help "filename" )

withInfo :: Parser a -> String -> ParserInfo a
withInfo p s = info (helper <*> p) (fullDesc  <> progDesc s)

parseArgs :: [String] -> ParserResult Args
parseArgs = execParserPure preferences parserInfo where
   parserInfo = withInfo argsParser "Advent of code"
   preferences = prefs (disambiguate <> showHelpOnEmpty <> showHelpOnError)
