{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Rope.Cursor where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

newtype Cursor = Cursor (Int, Int)
                 deriving (Show, Eq, Generic)

instance FromJSON Cursor
instance ToJSON Cursor

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

newPosition :: Position
newPosition = (0, newCursor)

positionIndex :: Position -> Int
positionIndex = fst

positionCursor :: Position -> Cursor
positionCursor = snd
