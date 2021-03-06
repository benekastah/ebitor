{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fprof-auto #-}
module Ebitor.Rope where

import Control.Applicative (pure)
import Data.Char
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Maybe
import Data.Monoid
import Data.String (IsString, fromString)
import qualified Data.Sequence as S

import Data.Aeson
import Data.FingerTree hiding (empty, null)
import Data.Text (Text)
import qualified Data.FingerTree as F
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Ebitor.Rope.Cursor


data Size = Size { sLength :: Int, sNumLines :: Int }
          deriving (Show, Eq)

instance Monoid Size where
    mempty = Size 0 0
    Size c l `mappend` Size c' l' = Size (c + c') (l + l')

instance Measured Size Chunk where
    measure (Chunk l t) = Size l $ countNL t

data Chunk = Chunk { cLength :: Int, text :: Text }
           deriving (Show, Eq)

newtype Rope = Rope { fromRope :: FingerTree Size Chunk }

instance Show Rope where
    show = show . unpackText

instance IsString Rope where
    fromString = pack

instance Monoid Rope where
    mempty = Ebitor.Rope.empty
    mappend = Ebitor.Rope.append
    mconcat = Ebitor.Rope.concat

instance FromJSON Rope where
    parseJSON = withText "String" $ pure . pack . T.unpack

instance ToJSON Rope where
    toJSON r = toJSON $ unpackText r

instance Eq Rope where
    a /= b = unpackText a /= unpackText b

countNL :: Text -> Int
countNL = T.count "\n"

textToChunk :: Text -> Chunk
textToChunk t = Chunk (T.length t) t

defaultChunkSize :: Int
defaultChunkSize = 1200

length :: Rope -> Int
length = sLength . F.measure . fromRope

numLines = sNumLines . F.measure . fromRope

null :: Rope -> Bool
null = (==0) . Ebitor.Rope.length

singleton :: Char -> Rope
singleton ch = pack [ch]

pack :: String -> Rope
pack = packText . T.pack

packWithSize :: Int -> String -> Rope
packWithSize size = packTextWithSize size . T.pack

unpack :: Rope -> String
unpack = T.unpack . unpackText

packText :: Text -> Rope
packText = packTextWithSize defaultChunkSize

packTextWithSize :: Int -> Text -> Rope
packTextWithSize size = Rope . F.fromList . map textToChunk . T.chunksOf size

unpackText :: Rope -> Text
unpackText = T.concat . map fromChunk . toList . fromRope
  where
    fromChunk (Chunk _ t) = t

empty :: Rope
empty = Rope F.empty

append :: Rope -> Rope -> Rope
append r r' = Rope $ fromRope r >< fromRope r'

concat :: [Rope] -> Rope
concat = foldl' append empty

splitAt :: Int -> Rope -> (Rope, Rope)
splitAt i r
    | i <= 0 = (empty, r)
    | i >= Ebitor.Rope.length r = (r, empty)
    | otherwise =
        let (a, b) = F.split ((> i) . sLength) $ fromRope r
            (a', b') = case viewl b of
                EmptyL -> (a, b)
                c :< b' ->
                    let (ca, cb) = splitChunkAt (i - Ebitor.Rope.length (Rope a)) c
                    in  (a |> ca, cb <| b')
        in  (Rope a', Rope b')
  where
    splitChunkAt i (Chunk _ t) =
        let (a, b) = T.splitAt i t
        in  (textToChunk a, textToChunk b)

take :: Int -> Rope -> Rope
take = (fst .) . Ebitor.Rope.splitAt

drop :: Int -> Rope -> Rope
drop = (snd .) . Ebitor.Rope.splitAt

findIndex :: (Char -> Bool) -> Rope -> Maybe Int
findIndex test r = findIndex' $ fromRope r
  where
    findIndex' r = case viewl r of
        EmptyL -> Nothing
        Chunk l t :< rest -> case T.findIndex test t of
            Nothing -> (l +) `fmap` findIndex' rest
            result -> result

findIndexEnd :: (Char -> Bool) -> Rope -> Maybe Int
findIndexEnd test r =
    fmap (Ebitor.Rope.length r -) (findIndex test $ Ebitor.Rope.reverse r)

findIndexFrom :: Int -> (Char -> Bool) -> Rope -> Maybe Int
findIndexFrom i test r =
    let (a, b) = Ebitor.Rope.splitAt i r
    in  fmap (+ (Ebitor.Rope.length a)) (findIndex test b)

findIndexEndFrom :: Int -> (Char -> Bool) -> Rope -> Maybe Int
findIndexEndFrom i test r =
    let (a, b) = Ebitor.Rope.splitAt i r
    in  findIndexEnd test a

takeWhile :: (Char -> Bool) -> Rope -> Rope
takeWhile f = Rope . takeWhile' . fromRope
  where
    takeWhile' r = case viewl r of
        EmptyL -> r
        chunk@(Chunk l t) :< rest ->
            let t' = T.takeWhile f t
                l' = T.length t'
            in  if l == l' then
                chunk <| takeWhile' rest
            else
                F.singleton $ Chunk l' t'


reverse :: Rope -> Rope
reverse = Rope . reverse' F.empty . fromRope
  where
    reverse' result r = case viewr r of
        EmptyR -> result
        rest :> Chunk l t ->
            reverse' (Chunk l (T.reverse t) <| result) rest


takeWhileEnd :: (Char -> Bool) -> Rope -> Rope
takeWhileEnd f = Rope . takeWhileEnd' . fromRope
  where
    takeWhileEnd' r = case viewr r of
        EmptyR -> r
        rest :> chunk@(Chunk l t) ->
            let t' = T.reverse $ T.takeWhile f $ T.reverse t
                l' = T.length t'
            in  if l == l' then
                takeWhileEnd' rest |> chunk
            else
                F.singleton $ Chunk l' t'

takeLine :: Rope -> (Rope, Rope)
takeLine r = takeLine' F.empty (fromRope r)
  where
    takeLine' ln r = case viewl r of
        EmptyL -> (Rope ln, Rope r)
        chunk@(Chunk l t) :< rest -> if sNumLines (F.measure chunk) > 0 then
            let (tLn, tRest) = T.break (=='\n') t
                ln' = case tLn of
                    "" -> ln
                    _ -> ln |> textToChunk tLn
                rest' = case T.drop 1 tRest of
                    "" -> rest
                    -- Since I think tLn will often be shorter than tRest, this
                    -- should be faster than textToChunk
                    tRest' -> Chunk (l - T.length tLn - 1) tRest' <| rest
            in  (Rope ln', Rope rest')
        else
            takeLine' (ln |> chunk) rest

linesSeq :: Rope -> S.Seq Rope
linesSeq r = linesSeq' S.empty r
  where
    linesSeq' seq r
        | Ebitor.Rope.null r = seq
        | otherwise =
            let (ln, r') = takeLine r
            in  linesSeq' (seq S.|> ln) r'

lines :: Rope -> [Rope]
lines r = toList $ linesSeq r

unlinesSeq :: S.Seq Rope -> Rope
unlinesSeq seq = Ebitor.Rope.unlines $ toList seq

unlines :: [Rope] -> Rope
unlines [] = empty
unlines (x:xs) = unlines' x xs
  where
    unlines' result [] = append result "\n"
    unlines' result (x:xs) = unlines' (Ebitor.Rope.concat [result, "\n", x]) xs

cons :: Char -> Rope -> Rope
cons ch r = insert r 0 ch

snoc :: Rope -> Char -> Rope
snoc r ch = insert r (Ebitor.Rope.length r) ch

uncons :: Rope -> Maybe (Char, Rope)
uncons r = case viewl (fromRope r) of
    EmptyL -> Nothing
    Chunk l t :< rest -> case T.uncons t of
        Just (ch, t') -> Just (ch, Rope (Chunk (l - 1) t' <| rest))
        Nothing -> uncons (Rope rest)

unsnoc :: Rope -> Maybe (Rope, Char)
unsnoc r = case viewr (fromRope r) of
    EmptyR -> Nothing
    rest :> Chunk l t -> case T.uncons $ T.reverse t of
        Just (ch, t') -> Just (Rope (rest |> Chunk (l - 1) (T.reverse t')), ch)
        Nothing -> unsnoc (Rope rest)

insert :: Rope -> Int -> Char -> Rope
insert r i = insertString r i . pure

insertString :: Rope -> Int -> String -> Rope
insertString r i = insertText r i . T.pack

insertText :: Rope -> Int -> Text -> Rope
insertText r i t =
    let (a, b) = Ebitor.Rope.splitAt i r
    in  Ebitor.Rope.concat [a, packText t, b]

getSlice :: Int -> Int -> (Int, Int)
getSlice start end
    | start < 0 = getSlice 0 end
    | end < 0 = getSlice start 0
    | start > end = getSlice end start
    | otherwise = (start, end)

slice :: Rope -> Int -> Int -> Rope
slice r start end
    | start == end = empty
    | otherwise = Ebitor.Rope.drop start $ Ebitor.Rope.take end r

remove :: Rope -> Int -> Int -> Rope
remove r start len
    | start < 0 = r
    | len == 0 = r
    | otherwise =
        let (a, b) = Ebitor.Rope.splitAt start r
        in  append a $ Ebitor.Rope.drop len b

-- Cursors
charWidth :: Char -> Int
charWidth '\t' = 8
charWidth '\n' = 0
charWidth '\r' = 0
charWidth '\v' = 0
charWidth '\f' = 0
charWidth c
    | isControl c = 0
    | isPrint c = 1

charHeight :: Char -> Int
charHeight '\n' = 1
charHeight '\r' = 1
charHeight _ = 0

positionForIndex :: Rope -> Int -> Position
positionForIndex r i
    | i < 0 = newPosition
    | otherwise =
        let (a, b) = Ebitor.Rope.splitAt i r
            lineNo = numLines a
            partialLine = takeWhileEnd (/= '\n') a
            colNo = foldl' (+) 0 $ map charWidth $ unpack partialLine
        in  (Ebitor.Rope.length a, Cursor (lineNo + 1, colNo + 1))

splitBeforeLine :: Rope -> Int -> (Rope, Rope)
splitBeforeLine r i =
    let (a, b) = F.split ((>= (i - 1)) . sNumLines) $ fromRope r
    in  (Rope a, Rope b)

advanceCursor :: Cursor -> Cursor -> Rope -> Rope -> (Cursor, Rope, Rope)
advanceCursor target@(Cursor (targetLn, targetCol)) curs@(Cursor (curLn, curCol)) prefix r
    | curs >= target || Ebitor.Rope.null r = (curs, prefix, r)
    | curLn < targetLn && numLines r > 0 =
        let (ln, r') = takeLine r
            prefix' = snoc (Ebitor.Rope.append prefix ln) '\n'
        in  advanceCursor target (Cursor (curLn + 1, 1)) prefix' r'
    | otherwise = case uncons r of
        Just (ch, r') ->
            let curLn' = curLn + charHeight ch
                curCol' = if curLn' /= curLn then 1 else curCol + charWidth ch
                prefix' = snoc prefix ch
                curs' = Cursor (curLn', curCol')
            in  if curLn' < targetLn then
                advanceCursor target (Cursor (curLn', curCol')) prefix' r'
            else if curLn' == targetLn && curCol' < targetCol then
                advanceCursor target curs' prefix' r'
            else if curLn == targetLn && curLn' /= targetLn then
                (curs, prefix, r)
            else
                (curs', prefix', r')
        Nothing -> undefined

positionForCursor :: Rope -> Cursor -> Position
positionForCursor r target@(Cursor (l, c))
    | l <= 0 = newPosition
    | c <= 0 = positionForCursor r $ Cursor (l, 1)
    | otherwise =
        let (a, b) = splitBeforeLine r l
            l' = numLines a + 1
            b' = takeWhileEnd (/= '\n') a
            (curs, a', _) = advanceCursor target (Cursor (l', 1)) empty (append b' b)
            i = Ebitor.Rope.length (append a a') - Ebitor.Rope.length b'
        in  (i, curs)
  where
    totalLines = numLines r

-- File operations
readFile :: FilePath -> IO Rope
readFile f = do
    t <- T.readFile f
    return $ packText t

writeFile :: FilePath -> Rope -> IO ()
writeFile f = T.writeFile f . unpackText
