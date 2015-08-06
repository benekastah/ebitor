{-# LANGUAGE OverloadedStrings #-}
module Ebitor.Rope where

import Data.Eq ()
import Data.List (foldl')
import Data.String (IsString, fromString)
import Prelude hiding (length, null, concat)
import qualified Prelude as P

import qualified Data.Text as T

fromRight :: Either a b -> b
fromRight e = case e of
    Right x -> x
    _ -> error "Cannot get Right from Left"

data Rope = Node Int Rope Rope | Leaf Int T.Text | Nil

instance Eq Rope where
    a /= b = unpack a /= unpack b

instance Show Rope where
    show r = unpack $ cons '"' $ snoc r '"'

data IndexError = IndexLessThanZero | IndexOutOfBounds
                  deriving (Show, Eq)

instance IsString Rope where
    fromString = pack

defaultChunkSize = 1000

length :: Rope -> Int
length (Node l _ _) = l
length (Leaf l _) = l
length Nil = 0

null :: Rope -> Bool
null Nil = True
null _ = False

-- Constructors
empty :: Rope
empty = Nil

singleton :: Char -> Rope
singleton = (Leaf 1) . T.singleton

pack :: String -> Rope
pack = packWithSize 0

packWithSize :: Int -> String -> Rope
packWithSize _ [] = Nil
packWithSize size str
    | size <= 0 = packWithSize defaultChunkSize str
    | len <= size = Leaf len text
    | otherwise =
        let (a, b) = splitAt (quot len 2) str
        in  Node len (packWithSize size a) (packWithSize size b)
  where
    len = P.length str
    text = T.pack str

-- Deconstructors
unpack :: Rope -> String
unpack Nil = ""
unpack (Leaf _ t) = T.unpack t
unpack (Node _ a b) = (unpack a) ++ (unpack b)

unpackText :: Rope -> T.Text
unpackText Nil = ""
unpackText (Leaf _ t) = t
unpackText (Node _ a b) = T.append (unpackText a) (unpackText b)

-- Modifying
append :: Rope -> Rope -> Rope
append a b = Node (length a + length b) a b

update :: Rope -> (Rope -> Int -> Rope) -> Int -> Either IndexError Rope
update r _ i
    | i < 0 = Left IndexLessThanZero
    | i > len = Left IndexOutOfBounds
    where len = length r
update (Node _ a b) f i
    | i <= lenA = case update a f i of
        Right a' -> Right $ append a' b
        e -> e
    | otherwise = case update b f (i - lenA) of
        Right b' -> Right $ append a b'
        e -> e
  where
    lenA = length a
update r f i = Right $ f r i

insert :: Rope -> Int -> Char -> Either IndexError Rope
insert r i ch = update r doUpdate i
  where
    doUpdate Nil _ = singleton ch
    doUpdate (Leaf l t) i =
        let (t1, t2) = T.splitAt i t
        in  Leaf (l + 1) (T.append t1 $ T.cons ch t2)

insertText :: Rope -> Int -> T.Text -> Either IndexError Rope
insertText r i t = update r doUpdate i
  where
    doUpdate Nil _ = Leaf (T.length t) t
    doUpdate (Leaf l t') i =
        let (t1, t2) = T.splitAt i t'
        in  Leaf (l + T.length t) $ T.concat [t1, t, t2]

insertString :: Rope -> Int -> String -> Either IndexError Rope
insertString r i s = insertText r i $ T.pack s

cons :: Char -> Rope -> Rope
cons ch r = fromRight $ insert r 0 ch

snoc :: Rope -> Char -> Rope
snoc r ch = fromRight $ insert r (length r) ch

concat :: [Rope] -> Rope
concat ropes = foldl' append empty ropes

-- Lookups
index :: Rope -> Int -> Either IndexError Char
index r i
    | i < 0 = Left IndexLessThanZero
    | i >= len = Left IndexOutOfBounds
    where len = length r
index (Node _ a b) i
    | i < lenA = index a i
    | otherwise = index b (i - lenA)
    where lenA = length a
index r@(Leaf l t) i = Right $ T.index t i
