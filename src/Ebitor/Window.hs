{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Window
    ( Orientation(..)
    , Rect(..)
    , Window(..)
    , (<->)
    , (<|>)
    , Ebitor.Window.map
    , focus
    , focusFirst
    , focusLast
    , focusNext
    , focusPrev
    , getFocused
    , getFocusedWindow
    , hasFocus
    , height
    , mapWindow
    , setRect
    , updateFocused
    , width
    , window
    ) where

import Data.Maybe
import GHC.Generics

import Data.Aeson

data Orientation = Horizontal | Vertical
            deriving (Generic, Show, Eq)
instance FromJSON Orientation
instance ToJSON Orientation

data Rect = Rect { rectX :: Int, rectY :: Int, rectWidth :: Int, rectHeight :: Int }
          deriving (Generic, Show, Eq)
instance FromJSON Rect
instance ToJSON Rect

data Window a = ContentWindow { cwContent :: a
                              , cwSize :: Maybe Int
                              , cwRect :: Maybe Rect
                              , cwHasFocus :: Bool
                              }
              | LayoutWindow { lwOrientation :: Orientation
                             , lwWindows :: [Window a]
                             , lwRect :: Maybe Rect
                             }
              deriving (Generic, Show, Eq)
instance FromJSON a => FromJSON (Window a)
instance ToJSON a => ToJSON (Window a)

window :: a -> Maybe Int -> Window a
window r s = ContentWindow r s Nothing False

hasFocus :: Window a -> Bool
hasFocus (ContentWindow {cwHasFocus = f}) = f
hasFocus _ = False

rect :: Window a -> Maybe Rect
rect (LayoutWindow { lwRect = s }) = s
rect (ContentWindow { cwRect = s }) = s

focus :: Eq a => Window a -> Window a -> Window a
focus q w@(LayoutWindow o wins s)
    | q == w && length wins > 0 = focus (head wins) w
    | otherwise = w { lwWindows = (Prelude.map (focus q) wins) }
focus q w@(ContentWindow {}) = w { cwHasFocus = q == w }

infixr 7 <->
(<->) :: Window a -> Window a -> Window a
w@(LayoutWindow Horizontal l _) <-> (LayoutWindow Horizontal r _) =
    unsize $ w { lwWindows = l ++ r }
w@(LayoutWindow Horizontal l _) <-> r = unsize $ w { lwWindows = l ++ [r] }
l <-> w@(LayoutWindow Horizontal r _) = unsize $ w { lwWindows = (l:r) }
l <-> r = unsize $ LayoutWindow Horizontal [l, r] Nothing

infixr 7 <|>
(<|>) :: Window a -> Window a -> Window a
w@(LayoutWindow Vertical l _) <|> (LayoutWindow Vertical r _) =
    unsize $ w { lwWindows = l ++ r }
w@(LayoutWindow Vertical l _) <|> r = unsize $ w { lwWindows = l ++ [r] }
l <|> w@(LayoutWindow Vertical r _) = unsize $ w { lwWindows = (l:r) }
l <|> r = unsize $ LayoutWindow Vertical [l, r] Nothing

unsize :: Window a -> Window a
unsize w@(LayoutWindow { lwWindows = wins }) =
    w { lwRect = Nothing, lwWindows = Prelude.map unsize wins }
unsize w@(ContentWindow {}) = w { cwRect = Nothing }

height _ (LayoutWindow { lwRect = Just (Rect { rectHeight = h }) }) = h
height _ (ContentWindow { cwRect = Just (Rect { rectHeight = h }) }) = h
height defaultH (LayoutWindow Horizontal wins _) = foldr ((+) . height defaultH) 0 wins
height _ (ContentWindow { cwSize = (Just h) }) = max h 0
height defaultH _ = defaultH

width _ (LayoutWindow { lwRect = Just (Rect { rectWidth = w }) }) = w
width _ (ContentWindow { cwRect = Just (Rect { rectWidth = w }) }) = w
width defaultW (LayoutWindow Vertical wins _) = foldr ((+) . width defaultW) 0 wins
width _ (ContentWindow { cwSize = (Just w) }) = max w 0
width defaultW _ = defaultW

minHeight = height 0
minWidth = width 0

setRect :: Window a -> Rect -> Window a
setRect w@(LayoutWindow o wins _) rect =
    w { lwWindows = sizedWins rect wins', lwRect = Just rect }
  where
    dimension = if o == Horizontal then rectHeight rect else rectWidth rect
    minSize = if o == Horizontal then minHeight else minWidth
    wins' = Prelude.map getWinSize wins
    getWinSize w = (minSize w, w)
    numUnsized = length [s | (s, _) <- wins', s <= 0]
    used = foldr ((+) . fst) 0 wins'
    leftover = dimension - used
    defaultSize = if numUnsized > 0 then max (leftover `quot` numUnsized) 0 else 0

    getRectPos rect = if o == Horizontal then rectY rect else rectX rect
    setRectPos rect p = if o == Horizontal then
        rect { rectY = p }
    else
        rect { rectX = p }
    setRectSize rect s = if o == Horizontal then
        rect { rectHeight = s }
    else
        rect { rectWidth = s }

    sizedWins _ [] = []
    sizedWins childRect ((s, w):wins') =
        let resizeTo = min (if s <= 0 then defaultSize else s)
                           (dimension - (getRectPos childRect))
            childRect' = setRectSize childRect resizeTo
            nextChildRect = setRectPos childRect' (getRectPos childRect' + resizeTo)
        in  (setRect w childRect'):(sizedWins nextChildRect wins')
setRect w@(ContentWindow {}) rect = w { cwRect = Just rect }

updateContent :: (a -> b) -> Window a -> Window b
updateContent f w@(ContentWindow { cwContent = c }) = w { cwContent = f c }

mapWindow :: (Window a -> Window b) -> Window a -> Window b
mapWindow f w@(LayoutWindow { lwWindows = wins }) =
    w { lwWindows = Prelude.map (mapWindow f) wins }
mapWindow f w = f w

map :: (a -> b) -> Window a -> Window b
map f = mapWindow (updateContent f)

updateWindowWhere :: (Window a -> Bool) -> (Window a -> Window a) -> Window a -> Window a
updateWindowWhere test f w = mapWindow doUpdate w
  where
    doUpdate w = if test w then f w else w

updateWhere :: (Window a -> Bool) -> (a -> a) -> Window a -> Window a
updateWhere test f = updateWindowWhere test (updateContent f)

updateFocused :: (a -> a) -> Window a -> Window a
updateFocused = updateWhere hasFocus

getWindowWhere :: (Window a -> Bool) -> Window a -> Maybe (Window a)
getWindowWhere test (LayoutWindow { lwWindows = wins }) = find wins
  where
    find [] = Nothing
    find (x:xs) = case getWindowWhere test x of
        result@(Just w) -> result
        Nothing -> find xs
getWindowWhere test w = if test w then Just w else Nothing

getWhere :: (Window a -> Bool) -> Window a -> Maybe a
getWhere test = fmap cwContent . getWindowWhere test

getFocusedWindow :: Window a -> Window a
getFocusedWindow = fromJust . getWindowWhere hasFocus

getFocused :: Window a -> a
getFocused = fromJust . getWhere hasFocus

firstContentWindow :: Window a -> Window a
firstContentWindow (LayoutWindow { lwWindows = wins }) = firstContentWindow $ head wins
firstContentWindow w = w

lastContentWindow :: Window a -> Window a
lastContentWindow (LayoutWindow { lwWindows = wins }) = lastContentWindow $ last wins
lastContentWindow w = w

focusLast :: Eq a => Window a -> Window a
focusLast w = focus (lastContentWindow w) w

focusFirst :: Eq a => Window a -> Window a
focusFirst w = focus (firstContentWindow w) w

focusPrev :: Eq a => Window a -> Window a
focusPrev w =
    case findPrev Nothing [w] of
        Just prev -> focus prev w
        Nothing -> focusLast w
  where
    findPrev :: Maybe (Window a) -> [Window a] -> Maybe (Window a)
    findPrev prev (w@(LayoutWindow { lwWindows = wins }):xs) =
        case findPrev prev wins of
            result@(Just _) -> result
            Nothing -> findPrev (Just $ lastContentWindow w) xs
    findPrev prev (w:xs)
        | hasFocus w = prev
        | otherwise = findPrev (Just w) xs
    findPrev _ _ = Nothing

focusNext :: Eq a => Window a -> Window a
focusNext w =
    case findNext Nothing [w] of
        Just next -> focus next w
        Nothing -> focusFirst w
  where
    findNext :: Maybe (Window a) -> [Window a] -> Maybe (Window a)
    findNext prev (w@(LayoutWindow { lwWindows = wins }):xs) =
        case findNext prev wins of
            result@(Just _) -> result
            Nothing -> findNext (Just $ lastContentWindow w) xs
    findNext (Just prev) (w@(ContentWindow {}):xs)
        | hasFocus prev = Just w
        | otherwise = findNext (Just w) xs
    findNext _ _ = Nothing
