{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.Rope.RegexTest (htf_thisModulesTests) where

import Test.Framework

import Text.Regex.TDFA
import Data.Array((!))

import Ebitor.Rope
import Ebitor.Rope.Regex
import Ebitor.RopeUtils
import Ebitor.Utils

(=~*) :: String -> String -> Rope
r1 =~* r2 = packRope r1 =~ packRope r2

test_match1 = assertEqual "asdf" ("asdf" =~* "^a.*")
test_match2 = assertEqual "hi there" ("hi there" =~* "[[:alpha:]]+[[:space:]][[:alpha:]]+")

test_noMatch1 = assertEqual "" ("asdf" =~* "^b.*")
test_noMatch2 = assertEqual "" ("hithere" =~* "[[:alpha:]]+[[:space:]][[:alpha:]]+")

test_replace = assertEqual expected (replace reg replacement haystack)
  where
    expected = "Hey man man"
    haystack = "Hey rude dude"
    reg = fromRight $ compileDefault ".ude"
    replacement = "man"

test_replaceOne = assertEqual expected (replaceOne reg replacement haystack)
  where
    expected = "Hey man dude"
    haystack = "Hey rude dude"
    reg = fromRight $ compileDefault ".ude"
    replacement = "man"


test_matchOnceEnd = assertEqual expected result
  where
    expected = Just (12, 7)
    haystack = pack' "chicken egg chicken egg"
    Right regex = compileDefault "chicken"
    result = matchOnceEnd regex haystack

test_matchOnceBefore = assertEqual expected result
  where
    expected = Just (44, 0)
    haystack = pack' "chicken egg chicken egg\nchicken egg chicken egg"
    Right regex = compileDefault "\\<"
    result = matchOnceBefore regex (Ebitor.Rope.length haystack) haystack
