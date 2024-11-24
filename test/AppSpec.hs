module AppSpec where

import App
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Simple test" $ do

     it "example-based unit test" $
        1 `shouldBe` 1

     prop "property-based unit test" $
        \l -> reverse ( reverse l ) == ( l::[Int])
