{-# LANGUAGE GADTs #-}
module Ebitor.Rope.Generic where

import Data.Eq ()
import Data.List (foldl')
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Ord (Ordering(..))
import Data.String (IsString, fromString)
import Prelude hiding (length, null, concat, splitAt, take, takeWhile, drop,
                       dropWhile, reverse, words, lines, unwords, unlines,
                       span, break)
import qualified Data.Foldable as F
import qualified Prelude as P

import qualified Data.Text as T

import Ebitor.Rope.Part (RopePart)
import qualified Ebitor.Rope.Part as RP
import Ebitor.Utils (fromRight)

newtype Cursor = Cursor (Int, Int)
                 deriving (Show, Eq)

instance Ord Cursor where
    compare (Cursor (ln, col)) (Cursor (ln', col'))
        | ln > ln' = GT
        | ln' > ln = LT
        | col > col' = GT
        | col' > col = LT
        | otherwise = EQ


data GenericRope a where
    Node :: Int -> Maybe Cursor -> GenericRope a -> GenericRope a -> GenericRope a
    Leaf :: RopePart a => Int -> Maybe Cursor -> a -> GenericRope a

instance Eq (GenericRope a) where
    a /= b = unpack a /= unpack b

instance RopePart a => Show (GenericRope a) where
    show = show . unpack

instance RopePart a => IsString (GenericRope a) where
    fromString = pack


data IndexError = IndexLessThanZero | IndexOutOfBounds
                  deriving (Show, Eq)

defaultChunkSize = 1000

length :: GenericRope a -> Int
length (Node l _ _ _) = l
length (Leaf l _ _) = l

cursor :: GenericRope a -> Maybe Cursor
cursor (Node _ c _ _) = c
cursor (Leaf _ c _) = c

null :: GenericRope a -> Bool
null (Leaf 0 _ _) = True
null _ = False

-- Constructors
empty :: RopePart a => GenericRope a
empty = Leaf 0 Nothing RP.empty

singleton :: RopePart a => Char -> GenericRope a
singleton = (Leaf 1 Nothing) . RP.singleton

pack :: RopePart a => String -> GenericRope a
pack = packWithSize 0

packWithSize :: RopePart a => Int -> String -> GenericRope a
packWithSize _ [] = empty
packWithSize size str
    | size <= 0 = packWithSize defaultChunkSize str
    | len <= size = Leaf len Nothing text
    | otherwise =
        let (a, b) = P.splitAt (quot len 2) str
        in  Node len Nothing (packWithSize size a) (packWithSize size b)
  where
    len = P.length str
    text = RP.pack str

-- Deconstructors
unpack :: GenericRope a -> String
unpack (Leaf _ _ t) = RP.unpack t
unpack (Node _ _ a b) = (unpack a) ++ (unpack b)

-- Modifying
append :: GenericRope a -> GenericRope a -> GenericRope a
append a b = Node (length a + length b) Nothing a b

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
update bf (Node _ _ a b) f i
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
    doUpdate (Leaf l _ t) i =
        let (t1, t2) = RP.splitAt i t
        in  Leaf (l + 1) Nothing (RP.append t1 $ RP.cons ch t2)

insertPart :: RopePart a => GenericRope a -> Int -> a -> Either IndexError (GenericRope a)
insertPart r i t = updateGt r doUpdate i
  where
    doUpdate (Leaf l _ t') i =
        let (t1, t2) = RP.splitAt i t'
        in  Leaf (l + RP.length t) Nothing $ RP.concat [t1, t, t2]

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
splitAt i (Node _ _ a b)
    | i < lenA =
        let (r1, r2) = splitAt i a
        in  (r1, append r2 b)
    | otherwise =
        let (r1, r2) = splitAt (i - lenA) b
        in  (append a r1, r2)
    where lenA = length a
splitAt i (Leaf len c t) =
    let (a, b) = RP.splitAt i t
    in  (Leaf i Nothing a, Leaf (len - i) c b)

take :: RopePart a => Int -> GenericRope a -> GenericRope a
take i = fst . splitAt i

takeWhile :: RopePart a => (Char -> Bool) -> GenericRope a -> GenericRope a
takeWhile f = pack . P.takeWhile f . unpack

drop :: RopePart a => Int -> GenericRope a -> GenericRope a
drop i = snd . splitAt i

dropWhile :: RopePart a => (Char -> Bool) -> GenericRope a -> GenericRope a
dropWhile f = pack . P.dropWhile f . unpack

span :: RopePart a => (Char -> Bool) -> GenericRope a -> (GenericRope a, GenericRope a)
span f r =
    let (a, b) = P.span f $ unpack r
    in  (pack a, pack b)

break :: RopePart a => (Char -> Bool) -> GenericRope a -> (GenericRope a, GenericRope a)
break f r =
    let (a, b) = P.break f $ unpack r
    in  (pack a, pack b)

uncons :: RopePart a => GenericRope a -> Maybe (Char, GenericRope a)
uncons (Node l _ a b) = case uncons a of
    Just (c, a') -> Just (c, Node (l - 1) Nothing a' b)
    Nothing -> Nothing
uncons (Leaf l _ t) = case RP.uncons t of
    Just (c, t') = Just (c, Leaf (l - 1) Nothing t')
    Nothing -> Nothing

reverse :: RopePart a => GenericRope a -> GenericRope a
reverse (Node l c a b) = Node l c (reverse b) (reverse a)
reverse (Leaf l c t) = Leaf l c $ RP.reverse t

lines :: RopePart a => GenericRope a -> [GenericRope a]
lines = map pack . P.lines . unpack

words :: RopePart a => GenericRope a -> [GenericRope a]
words = map pack . P.words . unpack

unlines :: RopePart a => [GenericRope a] -> GenericRope a
unlines = pack . P.unlines . map unpack

unwords :: RopePart a => [GenericRope a] -> GenericRope a
unwords = pack . P.unwords . map unpack

-- Lookups
index :: GenericRope a -> Int -> Either IndexError Char
index r i
    | i < 0 = Left IndexLessThanZero
    | i >= len = Left IndexOutOfBounds
    where len = length r
index (Node _ _ a b) i
    | i < lenA = index a i
    | otherwise = index b (i - lenA)
    where lenA = length a
index r@(Leaf l _ t) i = Right $ RP.index t i

-- Cursors
newCursor = Cursor (1, 1)

addToLn :: Int -> Cursor -> Cursor
addToLn i (Cursor (ln, col)) = Cursor (ln + i, col)

addToCol :: Int -> Cursor -> Cursor
addToCol i (Cursor (ln, col)) = Cursor (ln, col + i)

line :: Cursor -> Int
line (Cursor c) = fst c

column :: Cursor -> Int
column (Cursor c) = snd c

moveCursor :: RopePart a => Cursor -> a -> (Cursor, a)
moveCursor pos@(Cursor (ln, col)) s =
    case RP.uncons s of
        Just ('\r', s') -> case RP.uncons s' of
            Just ('\n', s'') -> (incLnCol pos, s'')
            _ -> (incLnCol pos, s')
        Just ('\n', s') -> (incLnCol pos, s')
        Just ('\t', s') -> (addToCol 8 pos, s')
        _ -> (incCol pos, s')
        Nothing -> (pos, s)
  where
    incLn = addToLn 1
    incCol = addToCol 1
    incLnCol (Cursor (ln, col)) = incLn $ Cursor (ln, 1)

lastCursorPos :: RopePart a => Cursor -> GenericRope a -> (Cursor, GenericRope a)
lastCursorPos _ r@(Node _ (Just c) _ _) = (c, r)
lastCursorPos _ r@(Leaf _ (Just c) _ _) = (c, r)
lastCursorPos c (Node l Nothing a b) =
    let (c', a') = lastCursorPos c a
        (c'', b') = lastCursorPos c' b
    in  (c'', Node l (Just c'') a' b')
lastCursorPos c (Leaf l Nothing t) =
    let c' = lastCursorPos' c t
    in  (c', Leaf l (Just c') t)
  where
    lastCursorPos' :: RopePart a => Cursor -> a -> Cursor
    lastCursorPos' pos s
        | RP.null s = pos
        | otherwise =
            let (pos', s') = moveCursor pos s
            in  lastCursorPos' pos' s'

indexForCursor :: RopePart a => GenericRope a -> Cursor -> (Int, GenericRope a)
indexForCursor r@(Node l _ a b) q = case lastCursorPos newCursor a of
    (c, a') | c == q -> (length a', Node l (cursor r) a' b)
    (c, a') | c > q ->
        let (i, a'') = indexForCursor a' q
        in  (i, Node l (cursor r) a'' b)
    (c, a') ->
        let (_, r') = lastCursorPos newCursor (Node l (cursor r) a' b)
            i = fst $ indexForCursor (right r')
        in  (i, r')
  where
    right (Node _ _ _ r) = r
indexForCursor r@(Leaf l (Just c) _) q
    | c > q = (l, r)
    | otherwise = error "nope"
