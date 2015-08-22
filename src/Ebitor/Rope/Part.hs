{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Ebitor.Rope.Part where

import Prelude hiding (concat, length, splitAt, take, drop, tail, reverse, null, uncons)
import qualified Prelude as P

import qualified Data.Text as T


class RopePart s where
    {-# MINIMAL pack, unpack #-}
    pack :: String -> s
    unpack :: s -> String

    empty :: s
    empty = pack ""

    append :: s -> s -> s
    append a b = concat [a, b]

    concat :: [s] -> s
    concat = pack . P.concat . map unpack

    cons :: Char -> s -> s
    cons ch = pack . (ch:) . unpack

    singleton :: Char -> s
    singleton ch = pack [ch]

    index :: s -> Int -> Char
    index s = (unpack s !!)

    length :: s -> Int
    length = P.length . unpack

    splitAt :: Int -> s -> (s, s)
    splitAt i s =
        let (a, b) = P.splitAt i $ unpack s
        in  (pack a, pack b)

    take :: Int -> s -> s
    take i = pack . P.take i . unpack

    drop :: Int -> s -> s
    drop i = pack . P.drop i . unpack

    uncons :: s -> Maybe (Char, s)
    uncons s =
        case P.uncons $ unpack s of
            Just (c, s') -> (c, pack s')
            Nothing -> Nothing

    reverse :: s -> s
    reverse = pack . P.reverse . unpack

    null :: s -> Bool
    null = P.null . unpack

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
    uncons = T.uncons
    reverse = T.reverse

    null = T.null
