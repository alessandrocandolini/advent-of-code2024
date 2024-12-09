{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Stats where

import Args (StatsRender (..))
import Control.Monad ((<=<))
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Aeson (ToJSON, toJSON)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types (Value)
import Data.Bifunctor (first)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Void (Void)
import qualified Env
import GHC.Generics (Generic)
import Network.HTTP.Req (
  GET (GET),
  NoReqBody (NoReqBody),
  bsResponse,
  defaultHttpConfig,
  header,
  https,
  req,
  responseBody,
  responseStatusCode,
  runReq,
  (/:),
 )
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec (MonadParsec (try), Parsec, anySingle, errorBundlePretty, manyTill, runParser)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (ParseErrorBundle)

newtype StarsCount = StarsCount {count :: Int}
  deriving (Eq, Show, Num) via Int

newtype Stats = Stats {starsCount :: StarsCount}
  deriving (Eq, Show)

renderSuccessOrError :: StatsRender -> Either StatsError Stats -> T.Text
renderSuccessOrError _ (Left (Not200 s)) = T.pack ("Unexpected HTTP status code response: " ++ show s)
renderSuccessOrError _ (Left (CannotFindNumberOfStars p)) = T.pack (errorBundlePretty p)
renderSuccessOrError ConsoleRender (Right stats) = consoleRender stats
renderSuccessOrError JsonRender (Right stats) = jsonRender stats

consoleRender :: Stats -> T.Text
consoleRender =
  ("stars:" <>)
    . T.pack
    . show
    . count
    . starsCount

jsonToText :: Value -> T.Text
jsonToText = TL.toStrict . TLE.decodeUtf8 . encodePretty

jsonRender :: Stats -> T.Text
jsonRender = jsonToText . toJSON . toShieldConfig

newtype StatsEnvVariables = StatsEnvVariables
  {session :: String}
  deriving (Eq, Show)

data ShieldsConfig = ShieldsConfig
  { schemaVersion :: Int
  , label :: String
  , message :: String
  , color :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON ShieldsConfig

defaultShieldsConfig :: String -> ShieldsConfig
defaultShieldsConfig m =
  ShieldsConfig
    { schemaVersion = 1
    , label = "stars ⭐️"
    , message = m
    , color = "green"
    }

toShieldConfig :: Stats -> ShieldsConfig
toShieldConfig =
  defaultShieldsConfig
    . show
    . count
    . starsCount

data StatsError
  = Not200 Int
  | CannotFindNumberOfStars ParsingError
  deriving (Eq, Show)

statsEnvVariablesParser :: Env.Parser Env.Error StatsEnvVariables
statsEnvVariablesParser =
  StatsEnvVariables
    <$> Env.var (Env.str <=< Env.nonempty) "AOC_SESSION" (Env.help "Value of the session cookie, used for login")

program :: Int -> StatsRender -> IO ()
program year render = do
  config <- Env.parse (Env.header "AOC stats requires env variables") statsEnvVariablesParser
  stats <- runExceptT $ program' year config
  T.putStrLn (renderSuccessOrError render stats) *> exit stats
 where
  exit (Left _) = exitFailure
  exit (Right _) = exitSuccess

program' :: Int -> StatsEnvVariables -> ExceptT StatsError IO Stats
program' year envVariables =
  ExceptT (uncurry parseIf200 <$> fetchPage year (session envVariables))

parseIf200 :: Int -> T.Text -> Either StatsError Stats
parseIf200 200 = first CannotFindNumberOfStars . parse
parseIf200 status = const (Left (Not200 status))

fetchPage :: Int -> String -> IO (Int, T.Text)
fetchPage year s = runReq defaultHttpConfig $ do
  let url = https "adventofcode.com" /: T.pack (show year)
      cookieHeader = B8.pack ("session=" ++ s)
  response <- req GET url NoReqBody bsResponse (header "Cookie" cookieHeader)
  let body = TE.decodeUtf8 $ responseBody response
      status = responseStatusCode response
  pure (status, body)

-- parsing
type Parser = Parsec Void T.Text
type ParsingError = ParseErrorBundle T.Text Void

starsCountParser :: Parser StarsCount
starsCountParser =
  StarsCount
    <$> ( manyTill anySingle (try (string "<span class=\"star-count\">"))
            *> decimal
            <* char '*'
            <* string "</span>"
        )

statsParser :: Parser Stats
statsParser = Stats <$> starsCountParser

parse :: T.Text -> Either ParsingError Stats
parse = runParser statsParser "html"
