{-# LANGUAGE QuasiQuotes #-}
module StatsSpec where

import Args (StatsRender(..))
import Stats ( parse , renderSuccessOrError, Stats(..))
import Test.Hspec ( describe, it, shouldBe, shouldSatisfy, Spec )
import Test.Hspec.QuickCheck ()
import Test.QuickCheck ()
import qualified Data.Text as T
import qualified Data.Text.IO as T
import SpecUtils (shouldBePretty)
import NeatInterpolation (trimming)
import Data.Either (isLeft)

exampleRender :: T.Text
exampleRender = [trimming|
{
    "color": "green",
    "label": "stars ⭐️",
    "message": "10",
    "schemaVersion": 1
}
|]

example :: T.Text
example = [trimming|
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
wrong = [trimming|
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

spec :: Spec
spec = describe "Stats" $ do

  it "json render" $ do
     renderSuccessOrError JsonRender (Right (Stats 10)) `shouldBe` exampleRender

  it "console render" $ do
     renderSuccessOrError ConsoleRender (Right (Stats 10)) `shouldBe` "stars:10"

  it "parse html" $ do
     parse wrong `shouldSatisfy` isLeft
