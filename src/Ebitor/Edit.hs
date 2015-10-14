{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Edit
    ( Editor(..)
    , TruncatedEditor()
    , backspace
    , cursorDown
    , cursorLeft
    , cursorMove
    , cursorRight
    , cursorToBOL
    , cursorToBOL'
    , cursorToBottom
    , cursorToEOL
    , cursorToTop
    , cursorUp
    , cursorWordLeft
    , cursorWordRight
    , editorCursor
    , editorIndex
    , insertChar
    , insertNewline
    , newEditor
    , tFilePath
    , tFirstLine
    , tPosition
    , tRope
    , truncateEditor
    ) where

import Data.Array((!))
import Data.Char
import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Ebitor.Rope (Rope)
import Ebitor.Rope.Cursor
import qualified Ebitor.Rope as R
import qualified Ebitor.Rope.Regex as R


data Editor = Editor
    { filePath :: Maybe FilePath
    , rope :: Rope
    , position :: Position
    , firstLine :: Int
    }
    deriving (Generic, Show, Eq)
instance FromJSON Editor
instance ToJSON Editor


newtype TruncatedEditor = TruncatedEditor Editor
                        deriving (Generic, Show, Eq)
instance FromJSON TruncatedEditor
instance ToJSON TruncatedEditor

getEditor :: TruncatedEditor -> Editor
getEditor (TruncatedEditor e) = e

tFilePath = filePath . getEditor
tRope = rope . getEditor
tPosition = position . getEditor
tFirstLine = firstLine . getEditor


newEditor :: Editor
newEditor = Editor { filePath = Nothing
                   , rope = R.empty
                   , position = newPosition
                   , firstLine = 1
                   }

truncateEditor :: Editor -> (Int, Int) -> TruncatedEditor
truncateEditor e (w, h) = TruncatedEditor $ e { rope = R.slice r 0 end }
  where
    r = R.unlines $ drop (firstLine e - 1) (R.lines $ rope e)
    end = fst $ R.positionForCursor r (Cursor (h, w + 1))

editorIndex = positionIndex . position
editorCursor = positionCursor . position

insertChar :: Char -> Editor -> Editor
insertChar c editor =
    let r = R.insert (rope editor) idx c
        p = R.positionForIndex r $ idx + 1
    in  editor { rope = r, position = p }
    where idx = editorIndex editor

insertString :: String -> Editor -> Editor
insertString s editor =
    let r = R.insertString (rope editor) idx s
        p = R.positionForIndex r $ idx + length s
    in  editor { rope = r, position = p }
    where idx = editorIndex editor

insertNewline :: Editor -> Editor
insertNewline = insertChar '\n'

deleteChar :: Int -> Editor -> Editor
deleteChar idx editor =
    let r = R.remove (rope editor) idx 1
        p = R.positionForIndex r $ idx
    in  editor { rope = r, position = p}

backspace :: Editor -> Editor
backspace editor = deleteBack editor
  where
    deleteBack =
        if crlf then
            deleteChar idx . deleteChar (idx - 1)
        else
            deleteChar idx
    idx = editorIndex editor - 1
    crlf = R.slice (rope editor) (idx - 1) (idx + 1) == "\r\n"

cursorMoveToIndex :: Int -> Editor -> Editor
cursorMoveToIndex i e =
    let (_, curs) = R.positionForIndex (rope e) i
    in  cursorMove (const curs) e

cursorMove :: (Cursor -> Cursor) -> Editor -> Editor
cursorMove f editor =
    let (_, c) = position editor
        p = R.positionForCursor (rope editor) (f c)
    in  editor { position = p }

cursorUp :: Editor -> Editor
cursorUp = cursorMove $ \(Cursor (ln, col)) -> Cursor (ln - 1, col)

cursorDown :: Editor -> Editor
cursorDown = cursorMove $ \(Cursor (ln, col)) -> Cursor (ln + 1, col)

cursorRight :: Editor -> Editor
cursorRight = cursorMove $ \(Cursor (ln, col)) -> Cursor (ln, col + 1)

cursorLeft :: Editor -> Editor
cursorLeft = cursorMove $ \(Cursor (ln, col)) -> Cursor (ln, col - 1)

cursorToNextMatch :: R.Regex -> Editor -> Editor
cursorToNextMatch regex editor =
    let i = fst . position $ editor
    in  case R.matchOnceFrom regex (i + 1) (rope editor) of
        Just (offset, _) -> cursorMoveToIndex offset editor
        Nothing -> editor

-- Not performant enough
cursorToPrevMatch :: R.Regex -> Editor -> Editor
cursorToPrevMatch regex editor =
    let i = fst . position $ editor
        r = fst $ R.splitAt (i - 1) (rope editor)
    in  case R.matchAll regex r of
        [] -> editor
        xs ->
            let match = last xs
                (offset, len) = match ! 0
            in  cursorMoveToIndex (offset + len) editor

cursorWordRight :: Editor -> Editor
cursorWordRight = cursorToNextMatch regex
  where
    Right regex = R.compileDefault "\\>"

cursorWordLeft :: Editor -> Editor
cursorWordLeft = cursorToPrevMatch regex
  where
    Right regex = R.compileFast "\\<"

cursorToTop :: Editor -> Editor
cursorToTop = cursorMoveToIndex 0

cursorToBottom :: Editor -> Editor
cursorToBottom editor = cursorMoveToIndex (R.length $ rope editor) editor

cursorToBOL :: Editor -> Editor
cursorToBOL = cursorMove $ \(Cursor (ln, col)) -> Cursor (ln, 1)

cursorToBOL' :: Editor -> Editor
cursorToBOL' editor =
    let Cursor (ln, _) = snd . position $ editor
        i = fst $ R.positionForCursor (rope editor) (Cursor (ln, 1))
        Right regex = R.compileFast "^[[:space:]]*"
    in  case R.matchOnceFrom regex i (rope editor) of
        Just (offset, len) -> cursorMoveToIndex (offset + len) editor
        Nothing -> editor

cursorToEOL :: Editor -> Editor
cursorToEOL = cursorToNextMatch regex
  where
    Right regex = R.compileFast "$"
