module Ebitor.RopeTest (htf_thisModulesTests) where

import Data.Either
import Data.List (foldl')
import Data.Maybe (fromJust)
import Prelude as P

import Test.Framework
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Text as T

import Ebitor.Rope as R
import Ebitor.Rope.Generic (GenericRope(..))
import Ebitor.Rope.Part (RopePart)
import Ebitor.RopeUtils
import Ebitor.Utils (fromRight)


test_empty = assertBool $ R.null empty


prop_length :: Rope -> Bool
prop_length r@(Leaf l t) = l == T.length t
prop_length r = sumChildLengths r == R.length r
  where
    sumChildLengths r@(Leaf _ _) = R.length r
    sumChildLengths (Node _ a b) = sumChildLengths a + sumChildLengths b

prop_packLength :: String -> Bool
prop_packLength s = P.length s == R.length (packRope s)

prop_appendLength :: Rope -> Rope -> Bool
prop_appendLength a b = R.length (R.append a b) == (R.length a) + (R.length b)


prop_append :: String -> String -> Bool
prop_append a b = unpack (R.append (packRope a) (packRope b)) == a ++ b


prop_unpack :: String -> Bool
prop_unpack s = unpack (packRope s) == s


prop_indexLessThanZero :: Rope -> Property
prop_indexLessThanZero r = forAll (suchThat arbitrary (<0)) $ \i ->
    index r i == Left IndexLessThanZero

prop_indexOutOfBounds :: Rope -> Property
prop_indexOutOfBounds r = forAll (suchThat arbitrary (>= (R.length r))) $ \i ->
    index r i == Left IndexOutOfBounds

prop_indexEmptyString :: Int -> Bool
prop_indexEmptyString i = isLeft $ index empty i

prop_index =
    forAll (suchThat arbitrary (/= "")) $ \s ->
    forAll (choose (0, (P.length s) - 1)) $ \i ->
    index (packRope s) i == Right (s !! i)


prop_insertIndexLessThanZero :: Rope -> Char -> Property
prop_insertIndexLessThanZero r ch = forAll (suchThat arbitrary (<0)) $ \i ->
    insert r i ch == Left IndexLessThanZero

prop_insertIndexOutOfBounds :: Rope -> Char -> Property
prop_insertIndexOutOfBounds r ch = forAll (suchThat arbitrary (> (R.length r))) $ \i ->
    insert r i ch == Left IndexOutOfBounds

prop_insert s ch = forAll (choose (0, (P.length s))) $ \i ->
    unpack (fromRight $ insert (packRope s) i ch) == insertStr s i
  where
    insertStr s i =
        let (s1, s2) = P.splitAt i s
        in  s1 ++ (ch:s2)


prop_insertTextIndexLessThanZero :: Rope -> T.Text -> Property
prop_insertTextIndexLessThanZero r t = forAll (suchThat arbitrary (<0)) $ \i ->
    insertText r i t == Left IndexLessThanZero

prop_insertTextIndexOutOfBounds :: Rope -> T.Text -> Property
prop_insertTextIndexOutOfBounds r t = forAll (suchThat arbitrary (> (R.length r))) $ \i ->
    insertText r i t == Left IndexOutOfBounds

prop_insertText s t = forAll (choose (0, (P.length s))) $ \i ->
    unpack (fromRight $ insertText (packRope s) i t) == insertStr s i
  where
    insertStr s i =
        let (s1, s2) = P.splitAt i s
        in  s1 ++ (T.unpack t) ++ s2


prop_cons ch s = ch:s == R.unpack (R.cons ch $ packRope s)
prop_snoc s ch = s++[ch] == R.unpack (R.snoc (packRope s) ch)
prop_singleton ch = packRope [ch] == R.singleton ch


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
