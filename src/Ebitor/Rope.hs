module Ebitor.Rope
    ( Cursor(..)
    , IndexError(..)
    , Position(..)
    , Rope
    , append
    , charWidth
    , concat
    , cons
    , drop
    , dropWhile
    , empty
    , head
    , index
    , init
    , insert
    , insertString
    , insertText
    , last
    , length
    , lines
    , newCursor
    , newPosition
    , null
    , pack
    , packWithSize
    , positionCursor
    , positionForCursor
    , positionForIndex
    , positionIndex
    , remove
    , reverse
    , singleton
    , slice
    , snoc
    , splitAt
    , tail
    , take
    , takeWhile
    , uncons
    , unlines
    , unpack
    , unpackText
    , unwords
    , words
    ) where

import Prelude hiding (length, null, concat, splitAt, take, takeWhile, drop,
                       dropWhile, reverse, words, lines, unwords, unlines,
                       head, tail, init, last)
import qualified Prelude as P

import qualified Data.Text as T

import Ebitor.Rope.Cursor (Cursor(..), Position, newCursor, newPosition,
                           positionIndex, positionCursor)
import Ebitor.Rope.Generic (GenericRope(..), IndexError(..), charWidth)
import qualified Ebitor.Rope.Cursor as R
import qualified Ebitor.Rope.Generic as R


type Rope = GenericRope T.Text


unpackText :: Rope -> T.Text
unpackText (Leaf _ _ t) = t
unpackText (Node _ _ a b) = T.append (unpackText a) (unpackText b)

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

remove :: Rope -> Int -> Either IndexError Rope
remove = R.remove

concat :: [Rope] -> Rope
concat = R.concat

splitAt :: Int -> Rope -> (Rope, Rope)
splitAt = R.splitAt

take :: Int -> Rope -> Rope
take = R.take

takeWhile :: (Char -> Bool) -> Rope -> Rope
takeWhile = R.takeWhile

drop :: Int -> Rope -> Rope
drop = R.drop

dropWhile :: (Char -> Bool) -> Rope -> Rope
dropWhile = R.dropWhile

reverse :: Rope -> Rope
reverse = R.reverse

uncons :: Rope -> Maybe (Char, Rope)
uncons = R.uncons

words :: Rope -> [Rope]
words = R.words

lines :: Rope -> [Rope]
lines = R.lines

unwords :: [Rope] -> Rope
unwords = R.unwords

unlines :: [Rope] -> Rope
unlines = R.unlines

head :: Rope -> Char
head = R.head

last :: Rope -> Char
last = R.last

init :: Rope -> Rope
init = R.init

tail :: Rope -> Rope
tail = R.tail

index :: Rope -> Int -> Either IndexError Char
index = R.index

slice :: Rope -> Int -> Int -> Rope
slice = R.slice

positionForCursor :: Rope -> Cursor -> (Position, Rope)
positionForCursor = R.positionForCursor

positionForIndex :: Rope -> Int -> (Position, Rope)
positionForIndex = R.positionForIndex
