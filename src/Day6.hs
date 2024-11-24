module Day6 where
import qualified Data.Text as T
import qualified Data.Text.IO as T

program :: FilePath -> IO ()
program = (=<<) print . fmap logic . T.readFile

data Answer = Answer deriving (Eq, Show)

logic :: T.Text -> Answer
logic = const Answer