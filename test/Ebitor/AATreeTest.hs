{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.AATreeTest (htf_thisModulesTests) where

import Prelude hiding (null)
import qualified Data.List as L
import Data.Foldable (toList)

import Test.Framework
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Ebitor.AATree.Internal

instance (Ord a, Arbitrary a) => Arbitrary (AATree a) where
    arbitrary = do
        ls <- listOf arbitrary
        return $ fromList ls

test_empty = assertEqual (empty :: AATree Int) Nil

prop_null :: AATree Int -> Bool
prop_null t@Nil = null t
prop_null t = not (null t)

-- Test AA tree laws. https://en.wikipedia.org/wiki/AA_tree#Balancing_rotations
prop_leafNodeLevel :: AATree Int -> Bool
prop_leafNodeLevel t@Nil = level t == 0
prop_leafNodeLevel t | leaf t = level t == 1
prop_leafNodeLevel _ = True

prop_leftNodeLevel :: AATree Int -> Bool
prop_leftNodeLevel (Branch lvT _ (Branch lvL _ _ _) _) = (lvT - 1) == lvL
prop_leftNodeLevel _ = True

prop_rightNodeLevel :: AATree Int -> Bool
prop_rightNodeLevel (Branch lvT _ _ (Branch lvR _ _ _)) =
    (lvT == lvR) || ((lvT - 1) == lvR)
prop_rightNodeLevel _ = True

prop_rightGrandchildNodeLevel :: AATree Int -> Bool
prop_rightGrandchildNodeLevel (Branch lv _ _ (Branch _ _ _ r)) = lv > level r
prop_rightGrandchildNodeLevel _ = True

prop_numChildren :: AATree Int -> Bool
prop_numChildren t | level t > 1 = not (null $ left t) && not (null $ right t)
prop_numChildren _ = True
-- End AA tree laws

prop_fromList :: [Int] -> Bool
prop_fromList ls = toList (fromList ls) == L.sort ls

prop_insert :: [Int] -> Bool
prop_insert ls = toList (insert 10 $ fromList ls) == L.insert 10 (L.sort ls)
