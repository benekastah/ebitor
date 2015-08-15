module Ebitor.Rope.RegexTest (htf_thisModulesTests) where

import Test.Framework

import Text.Regex.TDFA

import Ebitor.Rope
import Ebitor.Rope.Regex
import Ebitor.RopeUtils

(=~*) :: Rope -> Rope -> Rope
r1 =~* r2 = r1 =~ r2

test_match1 = assertEqual ("asdf" =~* "^a.*") "asdf"
test_match2 = assertEqual ("hi there" =~* "[[:alpha:]]+[[:space:]][[:alpha:]]+") "hi there"

test_noMatch1 = assertEqual ("asdf" =~* "^b.*") ""
test_noMatch2 = assertEqual ("hithere" =~* "[[:alpha:]]+[[:space:]][[:alpha:]]+") ""
