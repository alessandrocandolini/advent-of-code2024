module ArgsSpec where

import Args
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Options.Applicative (ParserResult(Success))
import Options.Applicative (ParserResult(Failure))

parseArgsMaybe = transform . parseArgs where
  transform (Success a) = Right a
  transform (Failure failure) = Left (show failure)
  transform _ = Left "completion"

spec :: Spec
spec = describe "Args parser" $ do

     it "is able to parse a valid command" $
        parseArgsMaybe ["-d", "1", "-f", "file"] `shouldBe` Right (Args 1 "file")
