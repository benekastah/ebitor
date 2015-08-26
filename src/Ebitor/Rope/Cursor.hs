module Ebitor.Rope.Cursor where

newtype Cursor = Cursor (Int, Int)
                 deriving (Show, Eq)

type Position = (Int, Cursor)

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

unwrap :: Cursor -> (Int, Int)
unwrap (Cursor c) = c

line :: Cursor -> Int
line = fst . unwrap

column :: Cursor -> Int
column = snd . unwrap

newPosition :: Int -> Cursor -> Position
newPosition = (,)

positionIndex :: Position -> Int
positionIndex = fst

positionCursor :: Position -> Cursor
positionCursor = snd
