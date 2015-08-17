module Ebitor.Rope.Cursor where

import Prelude hiding (splitAt, reverse, length)
import qualified Prelude as P

import Ebitor.Rope.Generic
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

newtype Position = Position (Cursor, Int)
                   deriving (Show, Eq)

instance Ord Position where
    compare (Position (_, i)) (Position (_, i')) = compare i i'

newCursor = Cursor (1, 1)

newPosition = Position (newCursor, 0)

addToLn :: Int -> Cursor -> Cursor
addToLn i (Cursor (ln, col)) = Cursor (ln + i, col)

addToCol :: Int -> Cursor -> Cursor
addToCol i (Cursor (ln, col)) = Cursor (ln, col + i)

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

-- Not optimized like positionForCursor since I don't anticipate using often when
-- not debugging. If it becomes a problem I'll fix it.
cursorForIndex :: RopePart a => GenericRope a -> Int -> Either IndexError Cursor
cursorForIndex r i
    | i < 0 = Left IndexLessThanZero
    | i >= length r = Left IndexOutOfBounds
    | otherwise =
        let (r', _) = splitAt i r
        in  Right $ lastCursorPos newCursor r'

-- Takes the current position, a rope and a (potentially invalid) cursor
-- position and returns the closest valid position.
positionForCursor :: RopePart a => Position -> GenericRope a -> Cursor -> Position
positionForCursor _ r (Cursor (ln, col)) | ln <= 0 = newPosition
positionForCursor curPos@(Position (curs, curI)) r newCurs =
    let (r1, r2) = splitAt curI r
    in  if curs <= newCurs
        then cursorToIndex 1 curPos $ unpack r2
        else cursorToIndex (-1) curPos $ unpack (reverse r1)
  where
    cmp sign = if sign == -1 then (<) else (>)
    cursorToIndex sign pos@(Position (curs, i)) s
        | curs == newCurs = pos
        | cmp sign curs newCurs = Position (curs, i)
        | s == "" = pos
        | otherwise =
            let (curs', s', chars) = moveCursor sign curs s
            in  cursorToIndex sign (Position (curs', i + chars * sign)) s'
