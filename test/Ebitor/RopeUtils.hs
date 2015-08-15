module Ebitor.RopeUtils where

import Test.Framework

import Test.QuickCheck.Arbitrary
import qualified Data.Text as T

import Ebitor.Rope as R
import Ebitor.Rope.Generic as GR
import Ebitor.Rope.Part (RopePart)

pack' :: RopePart a => String -> GenericRope a
pack' = GR.packWithSize 5

packRope :: String -> Rope
packRope = pack'

instance (RopePart a, Arbitrary a) => Arbitrary (GenericRope a) where
    arbitrary = do
        s <- arbitrary
        return $ pack' s

instance Arbitrary T.Text where
    arbitrary = do
        s <- arbitrary
        return $ T.pack s
