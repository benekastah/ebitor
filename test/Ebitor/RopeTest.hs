{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.RopeTest (htf_thisModulesTests) where

import Data.Either
import Data.List (foldl')
import Prelude as P

import Test.Framework
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen
import qualified Data.Text as T

import Ebitor.Rope as R

pack' :: String -> Rope
pack' = packWithSize 5

instance Arbitrary Rope where
    arbitrary = do
        s <- arbitrary
        return $ pack' s

instance Arbitrary T.Text where
    arbitrary = do
        s <- arbitrary
        return $ T.pack s


test_empty = assertEqual (empty :: R.Rope) Nil


prop_length :: Rope -> Bool
prop_length r@Nil = R.length r == 0
prop_length r@(Leaf l t) = l == T.length t
prop_length r = sumChildLengths r == R.length r
  where
    sumChildLengths r@Nil = R.length r
    sumChildLengths r@(Leaf _ _) = R.length r
    sumChildLengths (Node _ a b) = sumChildLengths a + sumChildLengths b

prop_packLength :: String -> Bool
prop_packLength s = P.length s == R.length (pack' s)

prop_appendLength :: Rope -> Rope -> Bool
prop_appendLength a b = R.length (R.append a b) == (R.length a) + (R.length b)


prop_append :: String -> String -> Bool
prop_append a b = unpack (R.append (pack' a) (pack' b)) == a ++ b


prop_unpack :: String -> Bool
prop_unpack s = unpack (pack s) == s


prop_indexLessThanZero r = forAll (suchThat arbitrary (<0)) $ \i ->
    index r i == Left IndexLessThanZero

prop_indexOutOfBounds r = forAll (suchThat arbitrary (>= (R.length r))) $ \i ->
    index r i == Left IndexOutOfBounds

prop_indexEmptyString :: Int -> Bool
prop_indexEmptyString i = isLeft $ index Nil i

prop_index =
    forAll (suchThat arbitrary (/= "")) $ \s ->
    forAll (choose (0, (P.length s) - 1)) $ \i ->
    index (pack' s) i == Right (s !! i)


prop_insertIndexLessThanZero r ch = forAll (suchThat arbitrary (<0)) $ \i ->
    insert r i ch == Left IndexLessThanZero

prop_insertIndexOutOfBounds r ch = forAll (suchThat arbitrary (> (R.length r))) $ \i ->
    insert r i ch == Left IndexOutOfBounds

prop_insert s ch = forAll (choose (0, (P.length s))) $ \i ->
    unpack (fromRight $ insert (pack' s) i ch) == insertStr s i
  where
    insertStr s i =
        let (s1, s2) = splitAt i s
        in  s1 ++ (ch:s2)


prop_insertTextIndexLessThanZero r t = forAll (suchThat arbitrary (<0)) $ \i ->
    insertText r i t == Left IndexLessThanZero

prop_insertTextIndexOutOfBounds r t = forAll (suchThat arbitrary (> (R.length r))) $ \i ->
    insertText r i t == Left IndexOutOfBounds

prop_insertText s t = forAll (choose (0, (P.length s))) $ \i ->
    unpack (fromRight $ insertText (pack' s) i t) == insertStr s i
  where
    insertStr s i =
        let (s1, s2) = splitAt i s
        in  s1 ++ (T.unpack t) ++ s2


prop_cons ch s = ch:s == R.unpack (R.cons ch $ pack' s)
prop_snoc s ch = s++[ch] == R.unpack (R.snoc (pack' s) ch)
prop_singleton ch = pack' [ch] == R.singleton ch


prop_concat ls = foldl' (++) "" ls == R.unpack (R.concat $ map pack' ls)
