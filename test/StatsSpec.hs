{-# LANGUAGE QuasiQuotes #-}

module StatsSpec where

import Args (StatsRender (..))
import Data.Either (isLeft)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Env
import NeatInterpolation (trimming)
import SpecUtils (shouldBePretty)
import Stats (Stats (..), StatsEnvVariables (..), StatsError (..), parse, parseIf200, renderSuccessOrError, statsEnvVariablesParser)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck ()
import Test.QuickCheck ()

exampleRender :: T.Text
exampleRender =
  [trimming|
{
    "color": "green",
    "label": "stars ⭐️",
    "message": "10",
    "schemaVersion": 1
}
|]

parsingErrorExample :: T.Text
parsingErrorExample =
  [trimming|
html:10:12:
   |
10 | </head><!--
   |            ^
unexpected end of input
expecting "<span class="star-count">"

|]

example :: T.Text
example =
  [trimming|
<!DOCTYPE html>
<html lang="en-us">
<head>
<meta charset="utf-8"/>
<title>Advent of Code 2024</title>
<link rel="stylesheet" type="text/css" href="/static/style.css?31"/>
<link rel="stylesheet alternate" type="text/css" href="/static/highcontrast.css?1" title="High Contrast"/>
<link rel="shortcut icon" href="/favicon.png"/>
<script>window.addEventListener('click', function(e,s,r){if(e.target.nodeName==='CODE'&&e.detail===3){s=window.getSelection();s.removeAllRanges();r=document.createRange();r.selectNodeContents(e.target);s.addRange(r);}});</script>
</head><!--
-->
<body>
<header><div><h1 class="title-global"><a href="/">Advent of Code</a></h1><nav><ul><li><a href="/2024/about">[About]</a></li><li><a href="/2024/events">[Events]</a></li><li><a href="https://cottonbureau.com/people/advent-of-code" target="_blank">[Shop]</a></li><li><a href="/2024/settings">[Settings]</a></li><li><a href="/2024/auth/logout">[Log Out]</a></li></ul></nav><div class="user">Alessandro Candolini <span class="star-count">10*</span></div></div><div><h1 class="title-event">&nbsp;&nbsp;&nbsp;<span class="title-event-wrap">var y=</span><a href="/2024">2024</a><span class="title-event-wrap">;</span></h1><nav><ul><li><a href="/2024">[Calendar]</a></li><li><a href="/2024/support">[AoC++]</a></li><li><a href="/2024/sponsors">[Sponsors]</a></li><li><a href="/2024/leaderboard">[Leaderboard]</a></li><li><a href="/2024/stats">[Stats]</a></li></ul></nav></div></header>
|]

wrong :: T.Text
wrong =
  [trimming|
<!DOCTYPE html>
<html lang="en-us">
<head>
<meta charset="utf-8"/>
<title>Advent of Code 2024</title>
<link rel="stylesheet" type="text/css" href="/static/style.css?31"/>
<link rel="stylesheet alternate" type="text/css" href="/static/highcontrast.css?1" title="High Contrast"/>
<link rel="shortcut icon" href="/favicon.png"/>
<script>window.addEventListener('click', function(e,s,r){if(e.target.nodeName==='CODE'&&e.detail===3){s=window.getSelection();s.removeAllRanges();r=document.createRange();r.selectNodeContents(e.target);s.addRange(r);}});</script>
</head><!--
|]

removeLastNewline :: T.Text -> T.Text
removeLastNewline text = fromMaybe text (T.stripSuffix "\n" text)

spec :: Spec
spec = describe "Stats" $ do
  it "render success in json" $ do
    renderSuccessOrError JsonRender (Right (Stats 10)) `shouldBe` exampleRender

  it "render success in console" $ do
    renderSuccessOrError ConsoleRender (Right (Stats 10)) `shouldBe` "stars:10"

  it "render HTML parsing error" $ do
    removeLastNewline (renderSuccessOrError ConsoleRender (parseIf200 200 wrong)) `shouldBe` parsingErrorExample

  it "render HTTP status code error" $ do
    renderSuccessOrError ConsoleRender (parseIf200 500 example) `shouldBe` "Unexpected HTTP status code response: 500"

  it "parse valid html" $ do
    parseIf200 200 example `shouldBe` Right (Stats 10)

  it "parse wrong html" $ do
    parseIf200 200 wrong `shouldSatisfy` isLeft

  it "do not parse html if status is not 200" $ do
    parseIf200 500 example `shouldBe` Left (Not200 500)

  it "parses valid environment variables" $ do
    let mockEnv = [("AOC_SESSION", "abc")]
    Env.parsePure statsEnvVariablesParser mockEnv
      `shouldBe` Right
        ( StatsEnvVariables
            { session = "abc"
            }
        )
  it "parse valid html" $ do
    parseIf200 200 example `shouldBe` Right (Stats 10)

  it "parse wrong html" $ do
    parseIf200 200 wrong `shouldSatisfy` isLeft

  it "do not parse html if status is not 200" $ do
    parseIf200 500 example `shouldBe` Left (Not200 500)

  it "parses valid environment variables" $ do
    let mockEnv = [("AOC_SESSION", "abc")]
    Env.parsePure statsEnvVariablesParser mockEnv
      `shouldBe` Right
        ( StatsEnvVariables
            { session = "abc"
            }
        )
