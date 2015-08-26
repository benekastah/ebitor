module Main where

import Graphics.Vty
import qualified Graphics.Vty.Picture as V

import Ebitor.Edit
import Ebitor.Rope (Rope)
import qualified Ebitor.Rope as R

renderEditor :: Vty -> Editor -> IO ()
renderEditor vty editor
    | R.null $ rope editor = return ()
    | otherwise = do
        let ropeLines = R.lines $ rope editor
            img = vertCat $ map (resizeHeight 1 . string defAttr . R.unpack) ropeLines
            R.Cursor (ln, col) = cursor editor
            pic = picForImage img
        update vty $ picForImage img
        setCursorPos (outputIface vty) (col - 1) (ln - 1)
        showCursor (outputIface vty)

insertChar :: Editor -> Char -> Editor
insertChar editor c =
    let ((i, R.Cursor (ln, col)), r) = R.positionForCursor (rope editor) (cursor editor)
        (Right r') = R.insert r i c
        curs = R.Cursor (ln, col + R.charWidth c)
    in  editor { rope = r', cursor = curs }

insertNewline :: Editor -> Editor
insertNewline editor =
    let ((i, R.Cursor (ln, col)), r) = R.positionForCursor (rope editor) (cursor editor)
        (Right r') = R.insert r i '\n'
        curs = R.Cursor (ln + 1, 1)
    in  editor { rope = r', cursor = curs }

backspace :: Editor -> Editor
backspace editor =
    let ((i, R.Cursor (ln, col)), r) = R.positionForCursor (rope editor) (cursor editor)
        (Right r') = R.remove r (i - 1)
        curs = R.Cursor (ln, col - 1)
    in  editor { rope = r', cursor = curs }

eventLoop :: Vty -> Editor -> IO ()
eventLoop vty editor = do
    renderEditor vty editor
    e <- nextEvent vty
    case e of
        EvKey KEsc [] -> return ()
        EvKey (KChar c) [] -> eventLoop vty $ insertChar editor c
        EvKey KEnter [] -> eventLoop vty $ insertNewline editor
        EvKey KBS [] -> eventLoop vty $ backspace editor
        _ -> eventLoop vty editor

main = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    setCursorPos (outputIface vty) 0 0
    showCursor (outputIface vty)
    let editor = Editor { filePath = Nothing, rope = R.empty, cursor = R.newCursor }
    eventLoop vty editor
    shutdown vty
