{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Window
    ( Orientation(..)
    , Window(..)
    , (<->)
    , (<|>)
    , focus
    , hasFocus
    , height
    , resize
    , width
    , window
    ) where

import GHC.Generics

import Data.Aeson

import qualified Ebitor.Rope as R

data Orientation = Horizontal | Vertical
            deriving (Generic, Show, Eq)
instance FromJSON Orientation
instance ToJSON Orientation

-- TODO change Int to Maybe Int here
data Window = ContentWindow R.Rope R.Cursor Int Bool
            | LayoutWindow Orientation [Window]
            deriving (Generic, Show, Eq)
instance FromJSON Window
instance ToJSON Window

window :: R.Rope -> R.Cursor -> Int -> Window
window r c s = ContentWindow r c s False

hasFocus :: Window -> Bool
hasFocus (ContentWindow _ _ _ f) = f
hasFocus _ = False

focus :: Window -> Window -> Window
focus q w@(LayoutWindow o wins)
    | q == w && length wins > 0 = focus (head wins) w
    | otherwise = LayoutWindow o $ map (focus q) wins
focus q w@(ContentWindow r c s _) = ContentWindow r c s $ q == w

infixr 7 <->
(<->) :: Window -> Window -> Window
(LayoutWindow Horizontal l) <-> (LayoutWindow Horizontal r) =
    LayoutWindow Horizontal $ l ++ r
(LayoutWindow Horizontal l) <-> r = LayoutWindow Horizontal $ l ++ [r]
l <-> (LayoutWindow Horizontal r) = LayoutWindow Horizontal (l:r)
l <-> r = LayoutWindow Horizontal [l, r]

infixr 7 <|>
(<|>) :: Window -> Window -> Window
(LayoutWindow Vertical l) <|> (LayoutWindow Vertical r) =
    LayoutWindow Vertical $ l ++ r
(LayoutWindow Vertical l) <|> r = LayoutWindow Vertical $ l ++ [r]
l <|> (LayoutWindow Vertical r) = LayoutWindow Vertical (l:r)
l <|> r = LayoutWindow Vertical [l, r]

height defaultH (LayoutWindow Horizontal wins) = foldr ((+) . height defaultH) 0 wins
height defaultH (LayoutWindow Vertical _) = defaultH
height _ (ContentWindow _ _ h _) = max h 0

width defaultW (LayoutWindow Vertical wins) = foldr ((+) . width defaultW) 0 wins
width defaultW (LayoutWindow Horizontal _) = defaultW
width _ (ContentWindow _ _ w _) = max w 0

minHeight = height 0
minWidth = width 0

resize :: Window -> (Int, Int) -> Window
resize (LayoutWindow o wins) (width, height) = LayoutWindow o $ sizedWins dimension wins'
  where
    dimension = if o == Horizontal then height else width
    getSize (LayoutWindow _ _) dim = if o == Horizontal then (width, dim) else (dim, height)
    getSize (ContentWindow _ _ _ _) dim = (dim, dim)
    minSize = if o == Horizontal then minHeight else minWidth
    wins' = map getWinSize wins
    getWinSize w = (minSize w, w)
    numUnsized = length [s | (s, _) <- wins', s <= 0]
    used = foldr ((+) . fst) 0 wins'
    leftover = dimension - used
    defaultSize = if numUnsized > 0 then max (leftover `quot` numUnsized) 0 else 0
    sizedWins _ [] = []
    sizedWins dimension ((s, w):[]) = [resize w $ getSize w dimension]
    sizedWins dimension ((s, w):wins') =
        let resizeTo = if s <= 0 then defaultSize else s
            resizeTo' = if dimension < resizeTo then dimension else resizeTo
        in  (resize w $ getSize w resizeTo'):(sizedWins (dimension - resizeTo') wins')
resize (ContentWindow r c _ f) (s, _) = ContentWindow r c s f
