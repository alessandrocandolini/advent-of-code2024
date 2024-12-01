module SpecUtils where

import Test.Hspec ( describe, it, shouldBe, Spec , Expectation, expectationFailure)
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, Stream, ShowErrorComponent, VisualStream, TraversableStream, Token)

-- Custom expectation function for ParseErrorBundle

shouldBePretty
  :: ( Stream s
     , VisualStream s
     , TraversableStream s
     , ShowErrorComponent e
     , Show e
     , Eq a
     , Show a
     )
  => Either (ParseErrorBundle s e) a
  -> Either String a
  -> Expectation
shouldBePretty actual expected =
  case (actual, expected) of
    (Left actualErr, Left expectedErr) ->
      let actualPretty = errorBundlePretty actualErr
      in if actualPretty == expectedErr
           then return ()
           else expectationFailure $ "Expected:\n" ++ expectedErr ++ "\n\nBut got:\n" ++ actualPretty
    (Right actualVal, Right expectedVal) ->
      actualVal `shouldBe` expectedVal
    (Left actualErr, Right _) ->
      expectationFailure $
        "Expected success but got error:\n" ++ errorBundlePretty actualErr
    (Right actualVal, Left expectedErr) ->
      expectationFailure $
        "Expected error:\n" ++ expectedErr ++ "\n\nBut got success:\n" ++ show actualVal

