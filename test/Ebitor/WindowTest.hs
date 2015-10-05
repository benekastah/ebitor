{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.WindowTest (htf_thisModulesTests) where

import Test.Framework

import Ebitor.Rope.Cursor
import Ebitor.Window as W

winA = window "a" Nothing
winB = window "b" Nothing
winB' = winB { cwSize = Just 1 }


layout o wins = LayoutWindow o wins Nothing
hozLayout wins = layout Horizontal wins
vertLayout wins = layout Vertical wins


test_horizCat1 = assertEqual expected result
  where
    expected = hozLayout [winA, winB]
    result = winA <-> winB
test_horizCat2 = assertEqual expected result
  where
    expected = hozLayout [winA, winB]
    result = hozLayout [winA] <-> winB
test_horizCat3 = assertEqual expected result
  where
    expected = hozLayout [winA, winB]
    result = winA <-> hozLayout [winB]
test_horizCat4 = assertEqual expected result
  where
    expected = hozLayout [winA, winB]
    result = hozLayout [winA] <-> hozLayout [winB]
test_horizCat5 = assertEqual expected result
  where
    expected = hozLayout [vertLayout [winA], winB]
    result = vertLayout [winA] <-> hozLayout [winB]
test_horizCat6 = assertEqual expected result
  where
    expected = hozLayout [winA, vertLayout [winB]]
    result = hozLayout [winA] <-> vertLayout [winB]


test_vertCat1 = assertEqual expected result
  where
    expected = vertLayout [winA, winB]
    result = winA <|> winB
test_vertCat2 = assertEqual expected result
  where
    expected = vertLayout [winA, winB]
    result = vertLayout [winA] <|> winB
test_vertCat3 = assertEqual expected result
  where
    expected = vertLayout [winA, winB]
    result = winA <|> vertLayout [winB]
test_vertCat4 = assertEqual expected result
  where
    expected = vertLayout [winA, winB]
    result = vertLayout [winA] <|> vertLayout [winB]
test_vertCat5 = assertEqual expected result
  where
    expected = vertLayout [hozLayout [winA], winB]
    result = hozLayout [winA] <|> vertLayout [winB]
test_vertCat6 = assertEqual expected result
  where
    expected = vertLayout [winA, hozLayout [winB]]
    result = vertLayout [winA] <|> hozLayout [winB]


test_setRect1 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal
                            [ winA { cwRect = Just (Rect 0 0 10 3) }
                            , winB' { cwRect = Just (Rect 0 3 10 1) }
                            ]
                            (Just (Rect 0 0 10 4))
    result = W.setRect (winA <-> winB') (Rect 0 0 10 4)
test_setRect2 = assertEqual expected result
  where
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 3 10) }
                            , winB' { cwRect = Just (Rect 3 0 1 10) }
                            ]
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB') (Rect 0 0 4 10)
test_setRect3 = assertEqual expected result
  where
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 2 10) }
                            , winB { cwRect = Just (Rect 2 0 2 10) }
                            ]
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB) (Rect 0 0 4 10)
test_setRect4 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal
                            [ winA { cwRect = Just (Rect 0 0 4 5) }
                            , winB { cwRect = Just (Rect 0 5 4 6) }
                            ]
                            (Just (Rect 0 0 4 11))
    result = W.setRect (winA <-> winB) (Rect 0 0 4 11)
test_setRect5 = assertEqual expected result
  where
    winB' = winB { cwSize = Just 4 }
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 0 10) }
                            , winB' { cwRect = Just (Rect 0 0 4 10) }
                            ]
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB') (Rect 0 0 4 10)
test_setRect6 = assertEqual expected result
  where
    winB' = winB { cwSize = Just 5 }
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 0 10) }
                            , winB' { cwRect = Just (Rect 0 0 4 10) }
                            ]
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB') (Rect 0 0 4 10)
