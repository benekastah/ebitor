{-# LANGUAGE MultiParamTypeClasses #-}
module Ebitor.Rope where

import Control.Applicative (pure)
import Data.Foldable (toList)
import Data.List (foldl')
import Data.Monoid
import Data.String (IsString, fromString)

import Data.Aeson
import Data.FingerTree hiding (empty, null)
import Data.Text (Text)
import qualified Data.FingerTree as F
import qualified Data.Text as T

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
    toJSON r = toJSON $ unpack r

instance Eq Rope where
    a /= b = unpack a /= unpack b

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

splitAtLine l r
    | l < 1 = (empty, r)
    | otherwise =
        let (a, b) = F.split ((>= l) . sNumLines) $ fromRope r
            (a', b') = case viewl b of
                EmptyL -> (a, b)
                (Chunk _ t) :< b' ->
                    let lns = T.lines t
                        (lnsA, lnsB) = Prelude.splitAt (l - sNumLines (measure a)) lns
                        ca = textToChunk $ T.unlines lnsA
                        cb = textToChunk $ T.unlines lnsB
                    in  (a |> ca, cb <| b')
        in  (Rope a', Rope b')


take :: Int -> Rope -> Rope
take = (fst .) . Ebitor.Rope.splitAt

drop :: Int -> Rope -> Rope
drop = (snd .) . Ebitor.Rope.splitAt

findIndex :: (Char -> Bool) -> Rope -> Maybe Int
findIndex f r = findIndex' $ fromRope r
  where
    findIndex' r = case viewl r of
        EmptyL -> Nothing
        Chunk _ t :< rest -> case T.findIndex f t of
            Nothing -> findIndex' rest
            result -> result

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

intercalate :: Rope -> [Rope] -> Rope
intercalate sep rs = intercalate' empty rs
  where
    intercalate' result [] = result
    intercalate' result (r:rs) = intercalate' (Ebitor.Rope.concat [result, sep, r]) rs

lines :: Rope -> [Rope]
lines r
    | Ebitor.Rope.null r = []
    | otherwise = linesT F.empty [] $ fromRope r
  where
    linesT ln lns r = case viewr r of
        EmptyR -> (Rope ln):lns
        rest :> chunk@(Chunk l t) -> case T.findIndex (== '\n') $ T.reverse t of
            Nothing -> linesT (chunk <| ln) lns rest
            Just i ->
                let (t', ln') = T.splitAt (l - i) t
                in  linesT F.empty ((Rope $ textToChunk ln' <| ln):lns) (r |> textToChunk t')

unlines :: [Rope] -> Rope
unlines = intercalate "\n"

cons :: Char -> Rope -> Rope
cons ch r = insert r 0 ch

snoc :: Rope -> Char -> Rope
snoc r ch = insert r (Ebitor.Rope.length r) ch

uncons :: Rope -> Maybe (Char, Rope)
uncons r
    | Ebitor.Rope.null r = Nothing
    | otherwise =
        let (a, b) = Ebitor.Rope.splitAt 1 r
        in  Just (head (unpack a), b)

unsnoc :: Rope -> Maybe (Rope, Char)
unsnoc r
    | Ebitor.Rope.null r = Nothing
    | otherwise =
        let (a, b) = Ebitor.Rope.splitAt (Ebitor.Rope.length r - 1) r
        in  Just (a, head (unpack b))

insert :: Rope -> Int -> Char -> Rope
insert r i = insertString r i . pure

insertString :: Rope -> Int -> String -> Rope
insertString r i = insertText r i . T.pack

insertText :: Rope -> Int -> Text -> Rope
insertText r i t =
    let (a, b) = Ebitor.Rope.splitAt (i + 1) r
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
    | len == 0 = r
    | otherwise =
        let (a, b) = Ebitor.Rope.splitAt start r
        in  append a $ Ebitor.Rope.drop len b

-- Cursors
charWidth :: Char -> Int
charWidth '\t' = 8
charWidth _ = 1

positionForIndex :: Rope -> Int -> Position
positionForIndex r i
    | i < 0 = newPosition
    | otherwise =
        let (a, b) = Ebitor.Rope.splitAt i r
            lineNo = numLines a
            partialLine = takeWhileEnd (/= '\n') a
            colNo = foldl' (+) 0 $ map charWidth $ unpack partialLine
        in  (Ebitor.Rope.length a, Cursor (lineNo, colNo))

positionForCursor :: Rope -> Cursor -> Position
positionForCursor r (Cursor (l, c))
    | l < 0 = newPosition
    | l > totalLines = positionForCursor r $ Cursor (totalLines, 1)
    | c < 0 = positionForCursor r $ Cursor (l, 1)
    | otherwise =
        let (a, b) = splitAtLine l r
            (col, a', b') = advanceCol 1 empty b
            i = Ebitor.Rope.length $ append a a'
        in  (i, Cursor (l, col))
  where
    totalLines = numLines r
    advanceCol c prefix r = case uncons r of
        Just (ch, r') ->
            let c' = c + charWidth ch
                prefix' = snoc prefix ch
            in  if c' >= c then
                (c', prefix', r')
            else
                advanceCol c' prefix' r'
        Nothing -> (c, prefix, r)

-- File operations
readFile :: FilePath -> IO Rope
readFile f = do
    s <- Prelude.readFile f
    return $ pack s

writeFile :: FilePath -> Rope -> IO ()
writeFile f = Prelude.writeFile f . unpack
