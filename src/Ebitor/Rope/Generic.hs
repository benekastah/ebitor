module Ebitor.Rope.Generic where

import Control.Applicative (pure)
import Data.Eq ()
import Data.List (foldl')
import Data.Maybe (isJust, fromJust, fromMaybe)
import Data.Ord (Ordering(..))
import Data.String (IsString, fromString)
import Prelude hiding (length, null, concat, splitAt, take, takeWhile, drop,
                       dropWhile, reverse, words, lines, unwords, unlines,
                       span, break, head, last, init, tail, readFile, writeFile,
                       appendFile)
import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Prelude as P

import Data.Aeson
import qualified Data.Text as T

import Ebitor.Rope.Cursor
import Ebitor.Rope.Part (RopePart)
import Ebitor.Utils (fromRight)
import qualified Ebitor.Rope.Part as RP

data GenericRope a = Node Int (Maybe Cursor) (GenericRope a) (GenericRope a)
                   | Leaf Int (Maybe Cursor) a

instance RopePart a => Eq (GenericRope a) where
    a /= b = unpack a /= unpack b

instance RopePart a => Show (GenericRope a) where
    show = show . unpack

instance RopePart a => IsString (GenericRope a) where
    fromString = pack

instance RopePart a => FromJSON (GenericRope a) where
    parseJSON = withText "String" $ pure . pack . T.unpack

instance RopePart a => ToJSON (GenericRope a) where
    toJSON r = toJSON $ unpack r


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
null = (0 ==) . length


right :: GenericRope a -> Maybe (GenericRope a)
right (Node _ _ _ r) = Just r
right _ = Nothing

left :: GenericRope a -> Maybe (GenericRope a)
left (Node _ _ l _) = Just l
left _ = Nothing

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
        in  append (packWithSize size a) (packWithSize size b)
  where
    len = P.length str
    text = RP.pack str

-- Deconstructors
unpack :: RopePart a => GenericRope a -> String
unpack (Leaf _ _ t) = RP.unpack t
unpack (Node _ _ a b) = (unpack a) ++ (unpack b)

-- Modifying
append :: RopePart a => GenericRope a -> GenericRope a -> GenericRope a
append a b = case uncons b of
    -- If \n is the first character of b, move it to the end of a. This ensures
    -- that \r\n will occur in the same node, a property our cursor functions
    -- rely on.
    Just (c@'\n', b') -> append (snoc a c) b'
    Just _ -> Node (length a + length b) (cursor a) a (removeCursorPos b)
    Nothing -> a

bounded :: (Int -> Int -> Bool) -> GenericRope a -> Int -> Maybe IndexError
bounded gt r i
    | i < 0 = Just IndexLessThanZero
    | i `gt` length r = Just IndexOutOfBounds
    | otherwise = Nothing

boundedGt :: GenericRope a -> Int -> Maybe IndexError
boundedGt = bounded (>)
boundedGte :: GenericRope a -> Int -> Maybe IndexError
boundedGte = bounded (>=)

type UpdateFn a = GenericRope a
               -> (GenericRope a -> Int -> GenericRope a)
               -> Int
               -> Either IndexError (GenericRope a)

update :: RopePart a => (GenericRope a -> Int -> Maybe IndexError) -> UpdateFn a
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

updateGt :: RopePart a => UpdateFn a
updateGt = update boundedGt
updateGte :: RopePart a => UpdateFn a
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
        in  (append a r1, removeCursorPos r2)
    where lenA = length a
splitAt i (Leaf len c t) =
    let (a, b) = RP.splitAt i t
        r1 = Leaf i c a
        r2 = Leaf (len - i) Nothing b
    in  (r1, r2)

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
uncons (Node _ _ a b)
    | null a = uncons b
    | otherwise = case uncons a of
        Just (c, a') -> Just (c, append a' b)
        Nothing -> Nothing
uncons (Leaf l _ t) = case RP.uncons t of
    Just (c, t') -> Just (c, Leaf (l - 1) Nothing t')
    Nothing -> Nothing

head :: RopePart a => GenericRope a -> Char
head (Node _ _ a _) = head a
head (Leaf _ _ t) = RP.head t

last :: RopePart a => GenericRope a -> Char
last (Node _ _ _ b) = last b
last (Leaf _ _ t) = RP.last t

init :: RopePart a => GenericRope a -> GenericRope a
init (Node _ _ a b)
    | null b = init a
    | otherwise = append a (init b)
init (Leaf l c t) = Leaf (l - 1) c $ RP.init t

tail :: RopePart a => GenericRope a -> GenericRope a
tail (Node _ _ a b)
    | null a = tail b
    | otherwise = append (tail a) b
tail (Leaf l c t) = Leaf (l - 1) c $ RP.tail t

reverse :: RopePart a => GenericRope a -> GenericRope a
reverse (Node l c a b) = append (reverse b) (reverse a)
reverse (Leaf l c t) = Leaf l c $ RP.reverse t

lines :: RopePart a => GenericRope a -> [GenericRope a]
lines r
    | null r = []
    | otherwise = case findIndex (\c -> c == '\r' || c == '\n') r of
        Just i ->
            let (x, xs) = splitAt i r
            in  x:(lines $ removeLeadingNewline xs)
        Nothing -> [r]
  where
    removeLeadingNewline r =
        if unpack (slice r 0 2) == "\r\n" then drop 2 r else drop 1 r

words :: RopePart a => GenericRope a -> [GenericRope a]
words = map pack . P.words . unpack

unlines :: RopePart a => [GenericRope a] -> GenericRope a
unlines = pack . P.unlines . map unpack

unwords :: RopePart a => [GenericRope a] -> GenericRope a
unwords = pack . P.unwords . map unpack

-- Lookups
index :: RopePart a => GenericRope a -> Int -> Either IndexError Char
index r i
    | i < 0 = Left IndexLessThanZero
    | i >= len = Left IndexOutOfBounds
    where len = length r
index (Node _ _ a b) i
    | i < lenA = index a i
    | otherwise = index b (i - lenA)
    where lenA = length a
index r@(Leaf _ _ t) i = Right $ RP.index t i

findIndex :: RopePart a => (Char -> Bool) -> GenericRope a -> Maybe Int
findIndex f = L.findIndex f . unpack

slice :: RopePart a => GenericRope a -> Int -> Int -> GenericRope a
slice r start end
    | start == end = empty
    | start < 0 = slice r 0 end
    | end < 0 = slice r start 0
    | start > end = slice r end start
    | start > length r = empty
    | otherwise = slice' r start end
  where
    slice' :: RopePart a => GenericRope a -> Int -> Int -> GenericRope a
    slice' (Node _ _ a b) start end
        | start < lenA && end <= lenA = sliceA
        | start < lenA && end > lenA = append sliceA sliceB
        | otherwise = sliceB
      where
        lenA = length a
        sliceA = slice' a start end
        sliceB = slice' b (start - lenA) (end - lenA)
    slice' r@(Leaf l _ t) start end
        | start < 0 = slice' r 0 end
        | end < 0 = slice' r start 0
        | start == 0 && end > l = r
        | otherwise = Leaf (RP.length part) Nothing part
        where part = RP.take (end - start) (RP.drop start t)

-- Cursors
charWidth :: Char -> Int
charWidth '\t' = 8
charWidth _ = 1

advancePosition :: RopePart a => Position -> a -> (Position, a)
advancePosition pos@(i, c@(Cursor (ln, col))) s =
    let i' = i + 1
    in  case RP.uncons s of
        Just ('\r', s') -> case RP.uncons s' of
            Just ('\n', s'') -> ((i' + 1, incLnCol c), s'')
            _ -> ((i', incLnCol c), s')
        Just ('\n', s') -> ((i', incLnCol c), s')
        Just (ch, s') -> ((i', addToCol (charWidth ch) c), s')
        Nothing -> (pos, s)
  where
    incLnCol (Cursor (ln, col)) = addToLn 1 $ Cursor (ln, 1)

lastCursorPos :: RopePart a => GenericRope a -> Cursor
lastCursorPos (Node l (Just c) a b) = lastCursorPos b
lastCursorPos (Leaf l (Just c) t) = lastCursorPos' c t
  where
    lastCursorPos' :: RopePart a => Cursor -> a -> Cursor
    lastCursorPos' pos s
        | RP.null s = pos
        | otherwise =
            let ((_, pos'), s') = advancePosition (0, pos) s
            in  lastCursorPos' pos' s'

buildCursorPos :: RopePart a => Cursor -> GenericRope a -> (Cursor, GenericRope a)
buildCursorPos _ r@(Node _ (Just c) _ _) = (c, r)
buildCursorPos _ r@(Leaf _ (Just c) _) = (c, r)
buildCursorPos c (Node l Nothing a b) =
    let (c', a') = buildCursorPos c a
        (_, b') = buildCursorPos (lastCursorPos a') b
    in  (c', Node l (Just c') a' b')
buildCursorPos c (Leaf l Nothing t) = (c, (Leaf l (Just c) t))

removeCursorPos :: RopePart a => GenericRope a -> GenericRope a
removeCursorPos (Node l _ a b) = append (removeCursorPos a) (removeCursorPos b)
removeCursorPos (Leaf l _ t) = Leaf l Nothing t


positionForCursor :: RopePart a => GenericRope a -> Cursor -> (Position, GenericRope a)
positionForCursor r (Cursor (l, c))
    | l <= 0 = ((0, Cursor (1, 1)), r)
    | c <= 0 = positionForCursor r (Cursor (l, 1))
positionForCursor r q | cursor r == Nothing =
    positionForCursor (snd $ buildCursorPos newCursor r) q
positionForCursor r@(Node l _ a b) q
    | cursorB <= q =
        let ((i, c), _) = positionForCursor b q
            p = (i + length a, c)
        in  (p, r)
    | otherwise =
        let (p, _) = positionForCursor a q
        in  (p, r)
  where
    cursorB = fromJust $ cursor b
positionForCursor r@(Leaf l (Just c) t) q
    | c > q = ((l, c), r)
    | otherwise = (findPosition (0, c) t, r)
  where
    findPosition :: RopePart a => Position -> a -> Position
    findPosition p@(_, c) t
        | c == q || RP.null t = p
        | line c == line q && column c > column q = p
        | otherwise =
            let (p'@(_, c'), t') = advancePosition p t
            in  if line c == line q && not (line c' == line q)
                then p
                else findPosition p' t'


positionForIndex :: RopePart a => GenericRope a -> Int -> (Position, GenericRope a)
positionForIndex r i | i < 0 = ((0, Cursor (1, 1)), r)
positionForIndex r q | cursor r == Nothing =
    positionForIndex (snd $ buildCursorPos newCursor r) q
positionForIndex r@(Node l _ a b) q
    | q < lenA =
        let (p, _) = positionForIndex a q
        in  (p, r)
    | otherwise =
        let ((i, c), _) = positionForIndex b $ q - lenA
            p = (i + lenA, c)
        in  (p, r)
    where lenA = length a
positionForIndex r@(Leaf l (Just c) t) q = (findPosition (0, c) t, r)
  where
    findPosition :: RopePart a => Position -> a -> Position
    findPosition p@(i, _) t
        | i >= q || RP.null t = p
        | otherwise =
            let (p', t') = advancePosition p t
            in  findPosition p' t'


readFile :: RopePart a => FilePath -> IO (GenericRope a)
readFile = fmap pack . P.readFile

writeFile :: RopePart a => FilePath -> GenericRope a -> IO ()
writeFile path = P.writeFile path . unpack

appendFile :: RopePart a => FilePath -> GenericRope a -> IO ()
appendFile path = P.appendFile path . unpack
