{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.RopeTest (htf_thisModulesTests) where

import Data.Either
import Data.List (foldl', intercalate, findIndex)
import Data.Maybe (fromJust)
import Prelude as P

import Test.Framework
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Text as T

import Ebitor.Rope as R
import Ebitor.RopeUtils


test_empty = assertBool $ R.null empty

prop_packLength :: String -> Bool
prop_packLength s = P.length s == R.length (packRope s)

prop_appendLength :: Rope -> Rope -> Bool
prop_appendLength a b = R.length (R.append a b) == (R.length a) + (R.length b)


prop_append :: String -> String -> Bool
prop_append a b = unpack (R.append (packRope a) (packRope b)) == a ++ b

test_empty_length = assertEqual 0 (R.length R.empty)

test_append_lf =
    assertEqual (append "qwerty" "\nasdf") (append "qwerty\n" "asdf")


prop_unpack :: String -> Bool
prop_unpack s = unpack (packRope s) == s


prop_insertIndexLessThanZero :: Rope -> Char -> Property
prop_insertIndexLessThanZero r ch = forAll (suchThat arbitrary (<0)) $ \i ->
    insert r i ch == insert r 0 ch

prop_insertIndexOutOfBounds :: Rope -> Char -> Property
prop_insertIndexOutOfBounds r ch = forAll (suchThat arbitrary (> (R.length r))) $ \i ->
    insert r i ch == insert r (R.length r) ch

prop_insert s ch = forAll (choose (0, (P.length s))) $ \i ->
    unpack (insert (packRope s) i ch) == insertStr s i
  where
    insertStr s i =
        let (s1, s2) = P.splitAt i s
        in  s1 ++ (ch:s2)


prop_insertTextIndexLessThanZero :: Rope -> T.Text -> Property
prop_insertTextIndexLessThanZero r t = forAll (suchThat arbitrary (<0)) $ \i ->
    insertText r i t == insertText r 0 t

prop_insertTextIndexOutOfBounds :: Rope -> T.Text -> Property
prop_insertTextIndexOutOfBounds r t = forAll (suchThat arbitrary (> (R.length r))) $ \i ->
    insertText r i t == insertText r (R.length r) t

prop_insertText s t = forAll (choose (0, (P.length s))) $ \i ->
    unpack (insertText (packRope s) i t) == insertStr s i
  where
    insertStr s i =
        let (s1, s2) = P.splitAt i s
        in  s1 ++ (T.unpack t) ++ s2


prop_cons ch s = ch:s == R.unpack (R.cons ch $ packRope s)
prop_snoc s ch = s++[ch] == R.unpack (R.snoc (packRope s) ch)
prop_singleton ch = packRope [ch] == R.singleton ch

prop_removeIndexLessThanZero :: Rope -> Property
prop_removeIndexLessThanZero r = forAll (suchThat arbitrary (<0)) $ \i ->
    remove r i 1 == r

prop_removeIndexOutOfBounds :: Rope -> Property
prop_removeIndexOutOfBounds r = forAll (suchThat arbitrary (>= (R.length r))) $ \i ->
    remove r i 1 == r

prop_removeLength =
    forAll (suchThat arbitrary (/= "")) $ \s ->
    forAll (choose (0, (P.length s - 1))) $ \i ->
        let r = packRope s
            r' = remove r i 1
        in  R.length r - 1 == R.length r'


test_remove1 = assertEqual (R.remove "hi there!" 0 1) "i there!"
test_remove2 =
    let r = "hi there!"
        len = R.length r - 1
    in  assertEqual (R.remove r len 1) "hi there"
test_remove3 = assertEqual (R.remove "hi there!" 2 1) "hithere!"


prop_concat ls = foldl' (++) "" ls == R.unpack (R.concat $ map packRope ls)


prop_splitAt s i =
    let (r1, r2) = R.splitAt i (packRope s)
        (s1, s2) = P.splitAt i s
    in  unpack r1 == s1 && unpack r2 == s2


prop_take s i = R.unpack (R.take i $ packRope s) == P.take i s
prop_drop s i = R.unpack (R.drop i $ packRope s) == P.drop i s

prop_uncons s@[] = R.uncons (packRope s) == Nothing
prop_uncons s1@(ch1:s2) =
    let (ch2, r) = fromJust $ R.uncons $ packRope s1
    in  ch1 == ch2 && R.unpack r == s2


-- prop_words s = map unpack (R.words $ pack s) == P.words s
prop_lines s = P.lines s == map unpack (R.lines $ packRope s)
-- prop_unwords s = unpack (R.unwords $ map pack s) == P.unwords s
prop_unlines s = unpack (R.unlines $ map pack s) == P.unlines s

-- prop_head = forAll (suchThat arbitrary ((>0) . P.length)) $ \s ->
--     R.head (packRope s) == P.head s
-- prop_last = forAll (suchThat arbitrary ((>0) . P.length)) $ \s ->
--     R.last (packRope s) == P.last s
-- prop_init = forAll (suchThat arbitrary ((>0) . P.length)) $ \s ->
--     R.unpack (R.init $ packRope s) == P.init s
-- prop_tail = forAll (suchThat arbitrary ((>0) . P.length)) $ \s ->
--     R.unpack (R.tail $ packRope s) == P.tail s

prop_slice s =
    forAll (suchThat arbitrary (>=0)) $ \start ->
    forAll (suchThat arbitrary (>=start)) $ \end ->
    P.take (end - start) (P.drop start s) == R.unpack (R.slice (packRope s) start end)

prop_sliceEmpty1 r i = "" == R.slice r i i
prop_sliceEmpty2 r = forAll (suchThat arbitrary (>= (R.length r))) $ \i ->
    "" == R.slice r i (i + 1)
