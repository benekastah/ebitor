{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Ebitor.Rope.Part where

import Prelude hiding (concat, length, splitAt, take, drop, tail)
import qualified Prelude as P

import qualified Data.Text as T


class RopePart s where
    {-# MINIMAL pack, unpack #-}
    pack :: String -> s
    unpack :: s -> String

    empty :: s
    append :: s -> s -> s
    concat :: [s] -> s
    cons :: Char -> s -> s
    singleton :: Char -> s

    index :: s -> Int -> Char
    length :: s -> Int
    splitAt :: Int -> s -> (s, s)
    take :: Int -> s -> s
    drop :: Int -> s -> s

    empty = pack ""
    append a b = concat [a, b]
    concat = pack . P.concat . map unpack
    cons ch = pack . (ch:) . unpack
    singleton ch = pack [ch]

    index s = (unpack s !!)
    length = P.length . unpack
    splitAt i s =
        let (a, b) = P.splitAt i $ unpack s
        in  (pack a, pack b)
    take i = pack . P.take i . unpack
    drop i = pack . P.drop i . unpack


instance RopePart String where
    pack = id
    unpack = id


instance RopePart T.Text where
    pack = T.pack
    unpack = T.unpack

    empty = T.empty
    append = T.append
    concat = T.concat
    cons = T.cons
    singleton = T.singleton

    index = T.index
    length = T.length
    splitAt = T.splitAt
    take = T.take
    drop = T.drop
