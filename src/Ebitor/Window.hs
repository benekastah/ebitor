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
    , rect
    , setRect
    , setSize
    , size
    , splitFocusedWindow
    , updateFocused
    , width
    , window
    ) where

import Data.List (intercalate)
import Data.Maybe
import GHC.Generics

import Data.Aeson

data Orientation = Horizontal | Vertical
            deriving (Generic, Show, Eq)
instance FromJSON Orientation
instance ToJSON Orientation

data Rect = Rect { rectX :: Int, rectY :: Int, rectWidth :: Int, rectHeight :: Int }
          deriving (Generic, Eq)
instance FromJSON Rect
instance ToJSON Rect

instance Show Rect where
    show r = intercalate " " ("Rect":coords)
      where
        coords = Prelude.map (\f -> show $ f r) [rectX, rectY, rectWidth, rectHeight]

data Window a = ContentWindow { cwContent :: a
                              , cwSize :: Maybe Int
                              , cwRect :: Maybe Rect
                              , cwHasFocus :: Bool
                              }
              | LayoutWindow { lwOrientation :: Orientation
                             , lwWindows :: [Window a]
                             , lwSize :: Maybe Int
                             , lwRect :: Maybe Rect
                             }
              deriving (Generic, Eq)
instance FromJSON a => FromJSON (Window a)
instance ToJSON a => ToJSON (Window a)

instance Show (Window a) where
    show w@(LayoutWindow {}) = intercalate " " [ "LayoutWindow"
                                               , show $ lwOrientation w
                                               , show $ lwWindows w
                                               , show $ lwRect w
                                               ]
    show w@(ContentWindow {}) =
        let s = intercalate " " ["ContentWindow", show $ cwRect w]
        in  if cwHasFocus w then "{*" ++ s ++ "*}" else s

window :: a -> Maybe Int -> Window a
window r s = ContentWindow r s Nothing False

hasFocus :: Window a -> Bool
hasFocus (ContentWindow {cwHasFocus = f}) = f
hasFocus _ = False

size :: Window a -> Maybe Int
size (LayoutWindow { lwSize = s }) = s
size (ContentWindow { cwSize = s }) = s

setSize :: Window a -> Maybe Int -> Window a
setSize w@(LayoutWindow {}) s = w { lwSize = s }
setSize w@(ContentWindow {}) s = w { cwSize = s }

rect :: Window a -> Maybe Rect
rect (LayoutWindow { lwRect = s }) = s
rect (ContentWindow { cwRect = s }) = s

focus :: Eq a => Window a -> Window a -> Window a
focus q w@(LayoutWindow { lwOrientation = o, lwWindows = wins })
    | q == w && length wins > 0 = focus (head wins) w
    | otherwise = w { lwWindows = (Prelude.map (focus q) wins) }
focus q w@(ContentWindow {}) = w { cwHasFocus = q == w }

infixr 7 <->
(<->) :: Window a -> Window a -> Window a
w@(LayoutWindow Horizontal l _ _) <-> (LayoutWindow Horizontal r _ _) =
    w { lwWindows = l ++ r }
w@(LayoutWindow Horizontal l _ _) <-> r = w { lwWindows = l ++ [r] }
l <-> w@(LayoutWindow Horizontal r _ _) = w { lwWindows = (l:r) }
l <-> r = LayoutWindow Horizontal [l, r] Nothing Nothing

infixr 7 <|>
(<|>) :: Window a -> Window a -> Window a
w@(LayoutWindow Vertical l _ _) <|> (LayoutWindow Vertical r _ _) =
    w { lwWindows = l ++ r }
w@(LayoutWindow Vertical l _ _) <|> r = w { lwWindows = l ++ [r] }
l <|> w@(LayoutWindow Vertical r _ _) = w { lwWindows = (l:r) }
l <|> r = LayoutWindow Vertical [l, r] Nothing Nothing

splitFocusedWindow :: Eq a => Orientation -> Window a -> Window a -> Window a
splitFocusedWindow o wins newWin = focus newWin (head $ doSplit Nothing [wins])
  where
    join = if o == Horizontal then (<->) else (<|>)
    doSplit _ (w@(LayoutWindow {}):xs) =
        (w { lwWindows = doSplit (Just w) (lwWindows w) }):xs
    doSplit (Just parent) (w@(ContentWindow {cwHasFocus = True}):xs) =
        if o == lwOrientation parent then w:newWin:xs else (w `join` newWin):xs
    doSplit Nothing (w@(ContentWindow {cwHasFocus = True}):xs) = (w `join` newWin):xs
    doSplit parent (x:xs) = x:(doSplit parent xs)


height defaultH (LayoutWindow _ wins (Just h) _) = max h 0
height defaultH (LayoutWindow Horizontal wins _ _) = foldr ((+) . height defaultH) 0 wins
height _ (ContentWindow { cwSize = (Just h) }) = max h 0
height defaultH _ = defaultH

width defaultW (LayoutWindow _ wins (Just w) _) = max w 0
width defaultW (LayoutWindow Vertical wins _ _) = foldr ((+) . width defaultW) 0 wins
width _ (ContentWindow { cwSize = (Just w) }) = max w 0
width defaultW _ = defaultW

minHeight = height 0
minWidth = width 0

setRect :: Window a -> Rect -> Window a
setRect w@(LayoutWindow o wins size _) rect =
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
    sizedWins childRect ((s, w):[]) =
        let resizeTo = dimension - getRectPos childRect
            childRect' = setRectSize childRect resizeTo
        in  [setRect w childRect']
    sizedWins childRect ((s, w):wins') =
        let remainder = dimension - getRectPos childRect
            resizeTo = min (if s <= 0 then defaultSize else s) remainder
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
    findNext Nothing (w@(ContentWindow {}):xs) = findNext (Just w) xs
    findNext _ _ = Nothing
