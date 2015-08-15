module Ebitor.Peano where

import Control.Monad
import Data.Maybe
import Data.Ord

data PositiveInt = One | Succ PositiveInt
                   deriving (Eq, Ord)

instance Show PositiveInt where
    show p = "PositiveInt " ++ show (toInt p :: Int)

instance Num PositiveInt where
    One + a = Succ a
    a + One = One + a
    (Succ a) + b@(Succ _) = a + Succ b

    One * b = b
    a * One = a
    a * (Succ b) = (a + a) * b

    One - _ = One
    (Succ x) - One = x
    (Succ a) - (Succ b) = a - b

    abs = id
    signum = const One
    fromInteger = fromJust . toPositive

toInt :: PositiveInt -> Int
toInt One = 1
toInt (Succ x) = 1 + toInt x

toPositive :: (Ord a, Num a) => a -> Maybe PositiveInt
toPositive 1 = Just One
toPositive x | x > 1 = liftM Succ (toPositive $ x - 1)
toPositive _ = Nothing
