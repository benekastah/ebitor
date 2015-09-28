{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.Rope.RegexTest (htf_thisModulesTests) where

import Test.Framework

import Text.Regex.TDFA

import Ebitor.Rope
import Ebitor.Rope.Regex (replace, replaceOne, compileDefault)
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
