{-# LANGUAGE GADTs #-}
module Ebitor.Rope.Generic where

import Data.Eq ()
import Data.List (foldl')
import Data.Maybe (isJust, fromJust)
import Data.String (IsString, fromString)
import Prelude hiding (length, null, concat, splitAt, take, drop)
import qualified Data.Foldable as F
import qualified Prelude as P

import qualified Data.Text as T

import Ebitor.Rope.Part (RopePart)
import qualified Ebitor.Rope.Part as RP
import Ebitor.Utils (fromRight)

data GenericRope a where
    Node :: Int -> GenericRope a -> GenericRope a -> GenericRope a
    Leaf :: RopePart a => Int -> a -> GenericRope a

instance Eq (GenericRope a) where
    a /= b = unpack a /= unpack b

instance RopePart a => Show (GenericRope a) where
    show r = unpack $ cons '"' $ snoc r '"'

instance RopePart a => IsString (GenericRope a) where
    fromString = pack


data IndexError = IndexLessThanZero | IndexOutOfBounds
                  deriving (Show, Eq)

defaultChunkSize = 1000

length :: GenericRope a -> Int
length (Node l _ _) = l
length (Leaf l _) = l

null :: GenericRope a -> Bool
null (Leaf 0 _) = True
null _ = False

-- Constructors
empty :: RopePart a => GenericRope a
empty = Leaf 0 RP.empty

singleton :: RopePart a => Char -> GenericRope a
singleton = (Leaf 1) . RP.singleton

pack :: RopePart a => String -> GenericRope a
pack = packWithSize 0

packWithSize :: RopePart a => Int -> String -> GenericRope a
packWithSize _ [] = empty
packWithSize size str
    | size <= 0 = packWithSize defaultChunkSize str
    | len <= size = Leaf len text
    | otherwise =
        let (a, b) = P.splitAt (quot len 2) str
        in  Node len (packWithSize size a) (packWithSize size b)
  where
    len = P.length str
    text = RP.pack str

-- Deconstructors
unpack :: GenericRope a -> String
unpack (Leaf _ t) = RP.unpack t
unpack (Node _ a b) = (unpack a) ++ (unpack b)

-- Modifying
append :: GenericRope a -> GenericRope a -> GenericRope a
append a b = Node (length a + length b) a b

bounded :: (Int -> Int -> Bool) -> GenericRope a -> Int -> Maybe IndexError
bounded gt r i
    | i < 0 = Just IndexLessThanZero
    | i `gt` length r = Just IndexOutOfBounds
    | otherwise = Nothing

boundedGt :: GenericRope a -> Int -> Maybe IndexError
boundedGt = bounded (>)
boundedGte :: GenericRope a -> Int -> Maybe IndexError
boundedGte = bounded (>=)

update :: (GenericRope a -> Int -> Maybe IndexError)
       -> GenericRope a
       -> (GenericRope a -> Int -> GenericRope a)
       -> Int
       -> Either IndexError (GenericRope a)
update bf r _ i | isJust err = Left $ fromJust err
    where err = bf r i
update bf (Node _ a b) f i
    | i <= lenA = case update bf a f i of
        Right a' -> Right $ append a' b
        e -> e
    | otherwise = case update bf b f (i - lenA) of
        Right b' -> Right $ append a b'
        e -> e
  where
    lenA = length a
update bf r f i = Right $ f r i

updateGt = update boundedGt
updateGte = update boundedGte

insert :: RopePart a => GenericRope a -> Int -> Char -> Either IndexError (GenericRope a)
insert r i ch = updateGt r doUpdate i
  where
    doUpdate (Leaf l t) i =
        let (t1, t2) = RP.splitAt i t
        in  Leaf (l + 1) (RP.append t1 $ RP.cons ch t2)

insertPart :: RopePart a => GenericRope a -> Int -> a -> Either IndexError (GenericRope a)
insertPart r i t = updateGt r doUpdate i
  where
    doUpdate (Leaf l t') i =
        let (t1, t2) = RP.splitAt i t'
        in  Leaf (l + RP.length t) $ RP.concat [t1, t, t2]

insertString :: RopePart a => GenericRope a -> Int -> String -> Either IndexError (GenericRope a)
insertString r i s = insertPart r i $ RP.pack s

cons :: RopePart a => Char -> GenericRope a -> GenericRope a
cons ch r = fromRight $ insert r 0 ch

snoc :: RopePart a => GenericRope a -> Char -> GenericRope a
snoc r ch = fromRight $ insert r (length r) ch

concat :: RopePart a => [GenericRope a] -> GenericRope a
concat ropes = foldl' append empty ropes

remove :: RopePart a => GenericRope a -> Int -> Either IndexError (GenericRope a)
remove r i = case boundedGte r i of
    Just e -> Left e
    Nothing -> Right $ append (take i r) (drop (i + 1) r)

splitAt :: RopePart a => Int -> GenericRope a -> (GenericRope a, GenericRope a)
splitAt i r
    | i <= 0 = (empty, r)
    | i >= length r = (r, empty)
splitAt i (Node _ a b)
    | i < lenA =
        let (r1, r2) = splitAt i a
        in  (r1, append r2 b)
    | otherwise =
        let (r1, r2) = splitAt (i - lenA) b
        in  (append a r1, r2)
    where lenA = length a
splitAt i (Leaf len t) =
    let (a, b) = RP.splitAt i t
    in  (Leaf i a, Leaf (len - i) b)

take :: RopePart a => Int -> GenericRope a -> GenericRope a
take i = fst . splitAt i

drop :: RopePart a => Int -> GenericRope a -> GenericRope a
drop i = snd . splitAt i

uncons :: RopePart a => GenericRope a -> Maybe (Char, GenericRope a)
uncons r
    | length r >= 1 =
        let (r1, r2) = splitAt 1 r
        in  Just (unpack r1 !! 0, r2)
    | otherwise = Nothing

-- Lookups
index :: GenericRope a -> Int -> Either IndexError Char
index r i
    | i < 0 = Left IndexLessThanZero
    | i >= len = Left IndexOutOfBounds
    where len = length r
index (Node _ a b) i
    | i < lenA = index a i
    | otherwise = index b (i - lenA)
    where lenA = length a
index r@(Leaf l t) i = Right $ RP.index t i
