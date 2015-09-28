{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.Rope.CursorTest (htf_thisModulesTests) where

import Data.List (intercalate)
import Data.Maybe (fromJust)

import Test.Framework

import Ebitor.Rope (Rope)
import Ebitor.Rope.Cursor
import Ebitor.RopeUtils
import qualified Ebitor.Rope as R

madWorld = "it's a mad mad mad mad world"

patchOfOldSnow =
    [ "\tA Patch of Old Snow" -- 20
    , "" -- 0
    , "There's a patch of old snow in a corner" -- 39
    , "That I should have guessed" -- 26
    , "Was a blow-away paper the rain" -- 30
    , "Had brought to rest." -- 20
    , "" -- 0
    , "It is speckled with grime as if" -- 31
    , "Small print overspread it," -- 26
    , "The news of a day I've forgotten--" -- 34
    , "If I ever read it." -- 18
    , "" -- 0
    , "- Robert Frost" -- 14
    ]

patchOfOldSnowCR = packRope $ intercalate "\r" patchOfOldSnow
patchOfOldSnowLF = packRope $ intercalate "\n" patchOfOldSnow
patchOfOldSnowCRLF = packRope $ intercalate "\r\n" patchOfOldSnow


test_positionForCursorCR =
    assertEqual (27, Cursor (3, 6))
                (R.positionForCursor patchOfOldSnowCR (Cursor (3, 6)))
test_positionForIndexCR =
    assertEqual (27, Cursor (3, 6))
                (R.positionForIndex patchOfOldSnowCR 27)


test_positionForCursorLF =
    assertEqual (27, Cursor (3, 6))
                (R.positionForCursor patchOfOldSnowLF (Cursor (3, 6)))
test_positionForIndexLF =
    assertEqual (27, Cursor (3, 6))
                (R.positionForIndex patchOfOldSnowLF 27)


test_positionForCursorCRLF =
    assertEqual (29, Cursor (3, 6))
                (R.positionForCursor patchOfOldSnowCRLF (Cursor (3, 6)))
test_positionForIndexCRLF =
    assertEqual (29, Cursor (3, 6))
                (R.positionForIndex patchOfOldSnowCRLF 29)


test_positionForCursorPastEOL =
    assertEqual (61, Cursor (3, 40))
                (R.positionForCursor patchOfOldSnowLF (Cursor (3, 100)))


test_positionForCursorAtEOL =
    assertEqual (61, Cursor (3, 40))
                (R.positionForCursor patchOfOldSnowLF (Cursor (3, 40)))


test_positionForCursorBeforeLine1 =
    assertEqual (0, Cursor (1, 1))
                (R.positionForCursor patchOfOldSnowLF (Cursor (1, 0)))


test_positionForCursorBeforeLine2 =
    assertEqual (0, Cursor (1, 1))
                (R.positionForCursor patchOfOldSnowLF (Cursor (1, -1000)))


test_positionForCursorInTab =
    assertEqual (1, Cursor (1, 9))
                (R.positionForCursor patchOfOldSnowLF (Cursor (1, 4)))
test_positionForIndexInCRLFSequence =
    assertEqual (22, Cursor (2, 1))
                (R.positionForIndex patchOfOldSnowCRLF 21)


test_positionForCursorBeforeDocument =
    assertEqual (0, Cursor (1, 1))
                (R.positionForCursor patchOfOldSnowLF (Cursor (0, 4)))
test_positionForIndexBeforeDocument =
    assertEqual (0, Cursor (1, 1))
                (R.positionForIndex patchOfOldSnowLF (-10))


test_positionForCursorPastDocument =
    assertEqual (R.length patchOfOldSnowLF, Cursor (13, 15))
                (R.positionForCursor patchOfOldSnowLF (Cursor (1000, 1)))
test_positionForIndexPastDocument =
    assertEqual (R.length patchOfOldSnowLF, Cursor (13, 15))
                (R.positionForIndex patchOfOldSnowLF (R.length patchOfOldSnowLF + 50))
