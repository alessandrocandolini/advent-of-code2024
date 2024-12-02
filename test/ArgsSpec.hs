module ArgsSpec where

import Args
import Options.Applicative (ParserResult (Failure, Success))
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

parseArgsMaybe = transform . parseArgs
 where
  transform (Success a) = Right a
  transform (Failure failure) = Left (show failure)
  transform _ = Left "completion"

spec :: Spec
spec = describe "Args parser" $ do
  it "is able to parse a valid command to run the solution" $
    parseArgsMaybe ["run", "-d", "1", "-f", "file"] `shouldBe` Right (Run (Args 1 "file"))

  it "is able to parse a valid command to generate the scaffolding for a new day" $
    parseArgsMaybe ["generate", "-d", "1"] `shouldBe` Right (Generate (GenerateArgs 1))
