{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Ebitor.Rope.CursorTest (htf_thisModulesTests) where

import Data.List (intercalate)
import Data.Maybe (fromJust)

import Test.Framework

import Ebitor.Rope (Rope)
import Ebitor.RopeUtils
import qualified Ebitor.Rope as R
import qualified Ebitor.Rope.Cursor as RC

lastCursorPos :: R.Cursor -> Rope -> R.Cursor
lastCursorPos = RC.lastCursorPos

madWorld = "it's a mad mad mad mad world"

patchOfOldSnow =
    [ "\tA Patch of Old Snow"
    , ""
    , "There's a patch of old snow in a corner"
    , "That I should have guessed"
    , "Was a blow-away paper the rain"
    , "Had brought to rest."
    , ""
    , "It is speckled with grime as if"
    , "Small print overspread it,"
    , "The news of a day I've forgotten--"
    , "If I ever read it."
    , ""
    , "- Robert Frost"
    ]

patchOfOldSnowCR = packRope $ intercalate "\r" patchOfOldSnow
patchOfOldSnowLF = packRope $ intercalate "\n" patchOfOldSnow
patchOfOldSnowCRLF = packRope $ intercalate "\r\n" patchOfOldSnow

test_lastCursorPos1 =
    assertEqual (R.Cursor (1, 29))
                (lastCursorPos R.newCursor $ packRope madWorld)
test_lastCursorPos2 =
    assertEqual (R.Cursor (2, 2))
                (lastCursorPos R.newCursor $ packRope (madWorld ++ "\nz"))
test_lastCursorPos3 =
    assertEqual (R.Cursor (2, 2))
                (lastCursorPos R.newCursor $ packRope (madWorld ++ "\rz"))
test_lastCursorPos4 =
    assertEqual (R.Cursor (2, 2))
                (lastCursorPos R.newCursor $ packRope (madWorld ++ "\r\nz"))
test_lastCursorPos5 =
    assertEqual (R.Cursor (2, 9))
                (lastCursorPos R.newCursor $ packRope (madWorld ++ "\r\n\t"))

test_cursorForIndex =
    assertEqual (Right $ R.Cursor (3, 6))
                (RC.cursorForIndex patchOfOldSnowCR 27)

test_positionForCursorForwardCR =
    assertEqual (R.Position (R.Cursor (3, 6), 27))
                (R.positionForCursor R.newPosition patchOfOldSnowCR (R.Cursor (3, 6)))
test_positionForCursorForwardLF =
    assertEqual (R.Position (R.Cursor (3, 6), 27))
                (R.positionForCursor R.newPosition patchOfOldSnowLF (R.Cursor (3, 6)))
test_positionForCursorForwardCRLF =
    assertEqual (R.Position (R.Cursor (3, 6), 29))
                (R.positionForCursor R.newPosition patchOfOldSnowCRLF (R.Cursor (3, 6)))

test_positionForCursorForwardPastEOL =
    assertEqual (R.Position (R.Cursor (3, 40), 61))
                (R.positionForCursor R.newPosition patchOfOldSnowLF (R.Cursor (3, 100)))
test_positionForCursorForwardInTab =
    assertEqual (R.Position (R.Cursor (1, 1), 0))
                (R.positionForCursor R.newPosition patchOfOldSnowLF (R.Cursor (1, 4)))

cursorPos88 = R.positionForCursor R.newPosition patchOfOldSnowCR $ R.Cursor (8, 8)
cursorPos88CRLF = R.positionForCursor R.newPosition patchOfOldSnowCRLF $ R.Cursor (8, 8)

test_positionForCursorBackCR =
    assertEqual (R.Position (R.Cursor (3, 6), 27))
                (R.positionForCursor cursorPos88 patchOfOldSnowCR (R.Cursor (3, 6)))
test_positionForCursorBackLF =
    assertEqual (R.Position (R.Cursor (3, 6), 27))
                (R.positionForCursor cursorPos88 patchOfOldSnowLF (R.Cursor (3, 6)))
test_positionForCursorBackCRLF =
    assertEqual (R.Position (R.Cursor (3, 6), 29))
                (R.positionForCursor cursorPos88CRLF patchOfOldSnowCRLF (R.Cursor (3, 6)))

test_positionForCursorBackPastEOL =
    assertEqual (R.Position (R.Cursor (3, 40), 61))
                (R.positionForCursor cursorPos88 patchOfOldSnowLF (R.Cursor (3, 100)))
test_positionForCursorBackInTab =
    assertEqual (R.Position (R.Cursor (1, 1), 0))
                (R.positionForCursor cursorPos88 patchOfOldSnowLF (R.Cursor (1, 4)))
