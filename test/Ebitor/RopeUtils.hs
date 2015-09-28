module Ebitor.RopeUtils where

import Test.Framework

import Test.QuickCheck.Arbitrary
import qualified Data.Text as T

import Ebitor.Rope as R

pack' :: String -> Rope
pack' = R.packWithSize 5

packRope :: String -> Rope
packRope = pack'

instance Arbitrary Rope where
    arbitrary = do
        s <- arbitrary
        return $ pack' s

instance Arbitrary T.Text where
    arbitrary = do
        s <- arbitrary
        return $ T.pack s
