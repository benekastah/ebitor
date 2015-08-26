{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.Rope.RegexTest (htf_thisModulesTests) where

import Test.Framework

import Text.Regex.TDFA

import Ebitor.Rope
import Ebitor.Rope.Regex
import Ebitor.RopeUtils

(=~*) :: String -> String -> Rope
r1 =~* r2 = packRope r1 =~ packRope r2

test_match1 = assertEqual "asdf" ("asdf" =~* "^a.*")
test_match2 = assertEqual "hi there" ("hi there" =~* "[[:alpha:]]+[[:space:]][[:alpha:]]+")

test_noMatch1 = assertEqual "" ("asdf" =~* "^b.*")
test_noMatch2 = assertEqual "" ("hithere" =~* "[[:alpha:]]+[[:space:]][[:alpha:]]+")
