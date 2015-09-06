{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Edit
    ( Editor(..)
    , backspace
    , editorCursor
    , editorIndex
    , insertChar
    , insertNewline
    ) where

import GHC.Generics

import Ebitor.Rope (Rope, Position, Cursor(..), positionIndex, positionCursor)
import qualified Ebitor.Rope as R

data Editor = Editor
    { filePath :: Maybe FilePath
    , rope :: Rope
    , position :: Position
    }
    deriving (Generic, Show)

unreachable = error "unreachable"

editorIndex = positionIndex . position
editorCursor = positionCursor . position

insertChar :: Char -> Editor -> Editor
insertChar c editor = case R.insert (rope editor) idx c of
    (Right r) ->
        let (p, r') = R.positionForIndex r $ idx + 1
        in  editor { rope = r', position = p }
    _ -> unreachable
    where idx = editorIndex editor

insertNewline :: Editor -> Editor
insertNewline = insertChar '\n'

backspace :: Editor -> Editor
backspace editor = case R.remove (rope editor) idx of
    (Right r) ->
        let (p, r') = R.positionForIndex r $ idx
            result = editor { rope = r', position = p }
        in  if crlf then backspace result else result
    _ -> editor
  where
    idx = editorIndex editor - 1
    crlf = R.slice (rope editor) (idx - 1) (idx + 1) == "\r\n"
