{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.WindowTest (htf_thisModulesTests) where

import Test.Framework

import Ebitor.Rope.Cursor
import Ebitor.Window as W

winA = window "a" newCursor 0
winB = window "b" newCursor 0


test_horizCat1 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal [winA, winB]
    result = winA <-> winB
test_horizCat2 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal [winA, winB]
    result = LayoutWindow Horizontal [winA] <-> winB
test_horizCat3 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal [winA, winB]
    result = winA <-> LayoutWindow Horizontal [winB]
test_horizCat4 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal [winA, winB]
    result = LayoutWindow Horizontal [winA] <-> LayoutWindow Horizontal [winB]
test_horizCat5 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal [LayoutWindow Vertical [winA], winB]
    result = LayoutWindow Vertical [winA] <-> LayoutWindow Horizontal [winB]
test_horizCat6 = assertEqual expected result
  where
    expected = LayoutWindow Horizontal [winA, LayoutWindow Vertical [winB]]
    result = LayoutWindow Horizontal [winA] <-> LayoutWindow Vertical [winB]


test_vertCat1 = assertEqual expected result
  where
    expected = LayoutWindow Vertical [winA, winB]
    result = winA <|> winB
test_vertCat2 = assertEqual expected result
  where
    expected = LayoutWindow Vertical [winA, winB]
    result = LayoutWindow Vertical [winA] <|> winB
test_vertCat3 = assertEqual expected result
  where
    expected = LayoutWindow Vertical [winA, winB]
    result = winA <|> LayoutWindow Vertical [winB]
test_vertCat4 = assertEqual expected result
  where
    expected = LayoutWindow Vertical [winA, winB]
    result = LayoutWindow Vertical [winA] <|> LayoutWindow Vertical [winB]
test_vertCat5 = assertEqual expected result
  where
    expected = LayoutWindow Vertical [LayoutWindow Horizontal [winA], winB]
    result = LayoutWindow Horizontal [winA] <|> LayoutWindow Vertical [winB]
test_vertCat6 = assertEqual expected result
  where
    expected = LayoutWindow Vertical [winA, LayoutWindow Horizontal [winB]]
    result = LayoutWindow Vertical [winA] <|> LayoutWindow Horizontal [winB]


test_resize1 = assertEqual expected result
  where
    win = winA <-> window "b" newCursor 1
    expected = LayoutWindow Horizontal [window "a" newCursor 3, window "b" newCursor 1]
    result = W.resize win (10, 4)
test_resize2 = assertEqual expected result
  where
    win = winA <|> window "b" newCursor 1
    expected = LayoutWindow Vertical [window "a" newCursor 3, window "b" newCursor 1]
    result = W.resize win (4, 10)
test_resize3 = assertEqual expected result
  where
    win = winA <|> winB
    expected = LayoutWindow Vertical [window "a" newCursor 2, window "b" newCursor 2]
    result = W.resize win (4, 10)
test_resize4 = assertEqual expected result
  where
    win = winA <-> winB
    expected = LayoutWindow Horizontal [window "a" newCursor 5, window "b" newCursor 6]
    result = W.resize win (4, 11)
test_resize5 = assertEqual expected result
  where
    win = winA <|> window "b" newCursor 4
    expected = LayoutWindow Vertical [window "a" newCursor 0, window "b" newCursor 4]
    result = W.resize win (4, 10)
test_resize6 = assertEqual expected result
  where
    win = winA <|> window "b" newCursor 5
    expected = LayoutWindow Vertical [window "a" newCursor 0, window "b" newCursor 4]
    result = W.resize win (4, 10)
