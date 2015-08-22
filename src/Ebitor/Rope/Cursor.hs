module Ebitor.Rope.Cursor where

import Prelude hiding (splitAt, reverse, length)
import qualified Prelude as P

import Ebitor.Rope.Generic as R
import Ebitor.Rope.Part (RopePart)
import qualified Ebitor.Rope.Part as RP

newtype Cursor = Cursor (Int, Int)
                 deriving (Show, Eq)

instance Ord Cursor where
    compare (Cursor (ln, col)) (Cursor (ln', col'))
        | ln > ln' = GT
        | ln' > ln = LT
        | col > col' = GT
        | col' > col = LT
        | otherwise = EQ

newCursor = Cursor (1, 1)

addToLn :: Int -> Cursor -> Cursor
addToLn i (Cursor (ln, col)) = Cursor (ln + i, col)

addToCol :: Int -> Cursor -> Cursor
addToCol i (Cursor (ln, col)) = Cursor (ln, col + i)

line :: Cursor -> Int
line (Cursor c) = fst c

column :: Cursor -> Int
column (Cursor c) = snd c

isNewline :: Char -> Bool
isNewline '\n' = True
isNewline '\r' = True
isNewline _ = False

moveCursor :: Int -> Cursor -> String -> (Cursor, String, Int)
moveCursor sign _ _ | sign /= -1 && sign /= 1 = error "sign must be 1 or -1"
moveCursor sign pos@(Cursor (ln, col)) s = case s of
    "" -> (pos, s, 0)
    '\r':'\n':s' -> (incLnCol pos s', s', 2)
    '\n':'\r':s' -> (incLnCol pos s', s', 2)
    '\r':s' -> (incLnCol pos s', s', 1)
    '\n':s' -> (incLnCol pos s', s', 1)
    '\t':s' -> (addToCol (8 * sign) pos, s', 1)
    _:s' -> (incCol pos, s', 1)
  where
    incLn = addToLn (1 * sign)
    incCol = addToCol (1 * sign)
    incLnCol (Cursor (ln, col)) s =
        let col' = if sign == -1
            then col + P.length (P.takeWhile (not . isNewline) s)
            else 1
        in  incLn $ Cursor (ln, col')

lastCursorPos :: RopePart a => Cursor -> GenericRope a -> Cursor
lastCursorPos pos (Node l a b) = lastCursorPos (lastCursorPos pos a) b
lastCursorPos pos (Leaf l t) = cursorPosFromString pos $ RP.unpack t
  where
    cursorPosFromString pos "" = pos
    cursorPosFromString pos s =
        let (pos', s', _) = moveCursor 1 pos s
        in  cursorPosFromString pos' s'
