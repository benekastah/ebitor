module Ebitor.Rope
    ( Rope
    , IndexError(..)
    , length
    , null
    , empty
    , singleton
    , pack
    , packWithSize
    , unpack
    , unpackText
    , append
    , insert
    , insertText
    , insertString
    , cons
    , snoc
    , concat
    , splitAt
    , take
    , drop
    , uncons
    , index
    ) where

import Prelude hiding (length, null, concat, splitAt, take, drop)
import qualified Prelude as P

import qualified Data.Text as T

import Ebitor.Rope.Generic (GenericRope(..), IndexError(..))
import qualified Ebitor.Rope.Generic as R


type Rope = GenericRope T.Text


unpackText :: Rope -> T.Text
unpackText (Leaf _ t) = t
unpackText (Node _ a b) = T.append (unpackText a) (unpackText b)

length :: Rope -> Int
length = R.length

null :: Rope -> Bool
null = R.null

empty :: Rope
empty = R.empty

singleton :: Char -> Rope
singleton = R.singleton

pack :: String -> Rope
pack = R.pack

packWithSize :: Int -> String -> Rope
packWithSize = R.packWithSize

unpack :: Rope -> String
unpack = R.unpack

append :: Rope -> Rope -> Rope
append = R.append

insert :: Rope -> Int -> Char -> Either IndexError Rope
insert = R.insert

insertText :: Rope -> Int -> T.Text -> Either IndexError Rope
insertText = R.insertPart

insertString :: Rope -> Int -> String -> Either IndexError Rope
insertString = R.insertString

cons :: Char -> Rope -> Rope
cons = R.cons

snoc :: Rope -> Char -> Rope
snoc = R.snoc

concat :: [Rope] -> Rope
concat = R.concat

splitAt :: Int -> Rope -> (Rope, Rope)
splitAt = R.splitAt

take :: Int -> Rope -> Rope
take = R.take

drop :: Int -> Rope -> Rope
drop = R.drop

uncons :: Rope -> Maybe (Char, Rope)
uncons = R.uncons

index :: Rope -> Int -> Either IndexError Char
index = R.index
