module Ebitor.Buffer where

import Data.Ord

import qualified Data.Text as T

import qualified Ebitor.AATree.Internal as A

newtype CursorPos = CursorPos (Int, Int)
                    deriving (Eq, Show)

instance Ord CursorPos where
    compare (lineA, colA) (lineB, colB)
        | lineA > lineB = GT
        | lineA < lineB = LT
        | colA > colB = GT
        | colA < colB = LT
        | otherwise = EQ

data BufferChunk = 
