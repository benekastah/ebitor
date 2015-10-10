{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.WindowTest (htf_thisModulesTests) where

import Test.Framework

import Ebitor.Rope.Cursor
import Ebitor.Window as W

winA = window "a" Nothing
winB = window "b" Nothing
winB' = winB { cwSize = Just 1 }
winC = window "c" Nothing


layout o wins = LayoutWindow o wins Nothing Nothing
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


test_splitFocusedWindow1 = assertEqual expected result
  where
    base = focus winA $ vertLayout [hozLayout [winA]]
    expected = focus winB $ vertLayout [hozLayout [vertLayout [winA, winB]]]
    result = splitFocusedWindow Vertical base winB
test_splitFocusedWindow2 = assertEqual expected result
  where
    base = focus winA $ vertLayout [hozLayout [winA]]
    expected = focus winB $ vertLayout [hozLayout [winA, winB]]
    result = splitFocusedWindow Horizontal base winB
test_splitFocusedWindow3 = assertEqual expected result
  where
    base = focus winA winA
    expected = focus winB $ hozLayout [winA, winB]
    result = splitFocusedWindow Horizontal base winB


test_setRect1 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal
                            [ winA { cwRect = Just (Rect 0 0 10 3) }
                            , winB' { cwRect = Just (Rect 0 3 10 1) }
                            ]
                            Nothing
                            (Just (Rect 0 0 10 4))
    result = W.setRect (winA <-> winB') (Rect 0 0 10 4)
test_setRect2 = assertEqual expected result
  where
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 3 10) }
                            , winB' { cwRect = Just (Rect 3 0 1 10) }
                            ]
                            Nothing
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB') (Rect 0 0 4 10)
test_setRect3 = assertEqual expected result
  where
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 2 10) }
                            , winB { cwRect = Just (Rect 2 0 2 10) }
                            ]
                            Nothing
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB) (Rect 0 0 4 10)
test_setRect4 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal
                            [ winA { cwRect = Just (Rect 0 0 4 5) }
                            , winB { cwRect = Just (Rect 0 5 4 6) }
                            ]
                            Nothing
                            (Just (Rect 0 0 4 11))
    result = W.setRect (winA <-> winB) (Rect 0 0 4 11)
test_setRect5 = assertEqual expected result
  where
    winB' = winB { cwSize = Just 4 }
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 0 10) }
                            , winB' { cwRect = Just (Rect 0 0 4 10) }
                            ]
                            Nothing
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB') (Rect 0 0 4 10)
test_setRect6 = assertEqual expected result
  where
    winB' = winB { cwSize = Just 5 }
    expected = LayoutWindow Vertical
                            [ winA { cwRect = Just (Rect 0 0 0 10) }
                            , winB' { cwRect = Just (Rect 0 0 4 10) }
                            ]
                            Nothing
                            (Just (Rect 0 0 4 10))
    result = W.setRect (winA <|> winB') (Rect 0 0 4 10)
test_setRect7 = assertEqual expected result
  where
    winL = (winB <|> winC) { lwSize = Just 1 }
    expected = LayoutWindow Horizontal
                            [ winA { cwRect = Just (Rect 0 0 9 9) }
                            , winL { lwWindows = [ winB { cwRect = Just (Rect 0 9 4 1) }
                                                 , winC { cwRect = Just (Rect 4 9 5 1) }
                                                 ]
                                   , lwRect = Just (Rect 0 9 9 1)
                                   }
                            ]
                            Nothing
                            (Just (Rect 0 0 9 10))
    result = W.setRect (winA <-> winL) (Rect 0 0 9 10)


test_focusPrevFirst = assertEqual expected result
  where
    base = focus winA $ hozLayout [ winA, winB ]
    expected = focus winB base
    result = focusPrev base
test_focusPrev = assertEqual expected result
  where
    base = focus winB $ hozLayout [ winA, winB ]
    expected = focus winA base
    result = focusPrev base
test_focusPrevAcrossBounds1 = assertEqual expected result
  where
    base = focus winB $ vertLayout [ hozLayout [winA], hozLayout [winB] ]
    expected = focus winA base
    result = focusPrev base
test_focusPrevAcrossBounds2 = assertEqual expected result
  where
    base = focus winB $ vertLayout [ winA, hozLayout [winB] ]
    expected = focus winA base
    result = focusPrev base
test_focusPrevAcrossBounds3 = assertEqual expected result
  where
    base = focus winB $ vertLayout [ hozLayout [winA], winB ]
    expected = focus winA base
    result = focusPrev base
test_focusPrevAcrossBounds4 = assertEqual expected result
  where
    base = focus winA $ vertLayout [ winA, hozLayout [winB, winC] ]
    expected = focus winC base
    result = focusPrev base
test_focusPrevAcrossBounds5 = assertEqual expected result
  where
    base = focus winB $ vertLayout [ winA, hozLayout [winB, winC] ]
    expected = focus winA base
    result = focusPrev base
test_focusPrevAcrossBounds6 = assertEqual expected result
  where
    base = focus winC $ vertLayout [ winA, hozLayout [winB, winC] ]
    expected = focus winB base
    result = focusPrev base


test_focusNextLast = assertEqual expected result
  where
    base = focus winB $ hozLayout [ winA, winB ]
    expected = focus winA base
    result = focusNext base
test_focusNext = assertEqual expected result
  where
    base = focus winA $ hozLayout [ winA, winB ]
    expected = focus winB base
    result = focusNext base
test_focusNextAcrossBounds1 = assertEqual expected result
  where
    base = focus winA $ vertLayout [ hozLayout [winA], hozLayout [winB] ]
    expected = focus winB base
    result = focusNext base
test_focusNextAcrossBounds2 = assertEqual expected result
  where
    base = focus winA $ vertLayout [ winA, hozLayout [winB] ]
    expected = focus winB base
    result = focusNext base
test_focusNextAcrossBounds3 = assertEqual expected result
  where
    base = focus winA $ vertLayout [ hozLayout [winA], winB ]
    expected = focus winB base
    result = focusNext base
test_focusNextAcrossBounds4 = assertEqual expected result
  where
    base = focus winA $ vertLayout [ winA, hozLayout [winB, winC] ]
    expected = focus winB base
    result = focusNext base
test_focusNextAcrossBounds5 = assertEqual expected result
  where
    base = focus winB $ vertLayout [ winA, hozLayout [winB, winC] ]
    expected = focus winC base
    result = focusNext base
test_focusNextAcrossBounds6 = assertEqual expected result
  where
    base = focus winC $ vertLayout [ winA, hozLayout [winB, winC] ]
    expected = focus winA base
    result = focusNext base
