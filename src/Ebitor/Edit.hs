{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Edit
    ( Editor(..)
    , backspace
    , cursorDown
    , cursorLeft
    , cursorMove
    , cursorRight
    , cursorUp
    , editorCursor
    , editorIndex
    , insertChar
    , insertNewline
    , newEditor
    ) where

import GHC.Generics

import Data.Aeson (FromJSON, ToJSON)

import Ebitor.Rope (Rope)
import Ebitor.Rope.Cursor
import qualified Ebitor.Rope as R

data Editor = Editor
    { filePath :: Maybe FilePath
    , rope :: Rope
    , position :: Position
    , firstLine :: Int
    }
    deriving (Generic, Show)

instance FromJSON Editor
instance ToJSON Editor

newEditor :: Editor
newEditor = Editor { filePath = Nothing
                   , rope = R.empty
                   , position = newPosition
                   , firstLine = 1
                   }

unreachable = error "unreachable"

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
