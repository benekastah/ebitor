{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.Rope.CursorTest (htf_thisModulesTests) where

import Data.List (intercalate)
import Data.Maybe (fromJust)

import Test.Framework

import Ebitor.Rope (Rope)
import Ebitor.Rope.Cursor
import Ebitor.Rope.Generic (buildCursorPos)
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
    assertEqual (27, R.Cursor (3, 6))
                (fst $ R.positionForCursor patchOfOldSnowCR (R.Cursor (3, 6)))

test_positionForCursorLF =
    assertEqual (27, R.Cursor (3, 6))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (3, 6)))

test_positionForCursorCRLF =
    assertEqual (29, R.Cursor (3, 6))
                (fst $ R.positionForCursor patchOfOldSnowCRLF (R.Cursor (3, 6)))


positionForCursorCRLF = fst $ R.positionForCursor text (R.Cursor (3, 6))
  where
    text =
        let (_, r) = buildCursorPos newCursor patchOfOldSnowCRLF
        in  r

test_positionForCursorPastEOL =
    assertEqual (61, R.Cursor (3, 40))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (3, 100)))

test_positionForCursorAtEOL =
    assertEqual (61, R.Cursor (3, 40))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (3, 40)))

test_positionForCursorBeforeLine1 =
    assertEqual (0, R.Cursor (1, 1))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (1, 0)))

test_positionForCursorBeforeLine2 =
    assertEqual (0, R.Cursor (1, 1))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (1, -1000)))

test_positionForCursorInTab =
    assertEqual (1, R.Cursor (1, 9))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (1, 4)))

test_positionForCursorBeforeDocument =
    assertEqual (0, R.Cursor (1, 1))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (0, 4)))

test_positionForCursorPastDocument =
    assertEqual (R.length patchOfOldSnowLF, R.Cursor (13, 15))
                (fst $ R.positionForCursor patchOfOldSnowLF (R.Cursor (1000, 1)))
