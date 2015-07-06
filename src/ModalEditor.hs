module ModalEditor
    ( modalEditWidget
    ) where

import Control.Monad

import Data.Text
import Graphics.Vty
import Graphics.Vty.Widgets.All hiding (applyEdit)
import qualified Graphics.Vty.Widgets.Edit as E
import qualified Graphics.Vty.Widgets.TextZipper as Z

import Commander

type KeyHandler = Widget ModalEdit -> Key -> [Modifier] -> IO Bool

data EditMode = NormalMode | InsertMode
                deriving (Show)

data ModalEdit = ModalEdit {
    normalKeyHandler :: KeyHandler,
    insertKeyHandler :: KeyHandler,
    mode :: EditMode,
    editor :: Widget Edit,
    commander :: Widget CommandEdit
}

instance Show ModalEdit where
    show e = "ModalEdit " ++ show (mode e)

setMode :: Widget ModalEdit -> EditMode -> IO ()
setMode this m = updateWidgetState this $ \st -> st {mode = m}

applyEdit :: (TextZipper Text -> TextZipper Text) -> Widget ModalEdit -> IO ()
applyEdit action this = getEditor this >>= E.applyEdit action

normalKeyHandler_ :: KeyHandler
normalKeyHandler_ this key mod =
    case (key, mod) of
        (KChar 'i', []) -> setMode this InsertMode >> return True
        (KChar 'h', []) -> doEdit Z.moveLeft
        (KChar 'j', []) -> doEdit Z.moveDown
        (KChar 'k', []) -> doEdit Z.moveUp
        (KChar 'l', []) -> doEdit Z.moveRight
        (KChar '$', []) -> doEdit Z.gotoEOL
        (KChar '^', []) -> doEdit Z.gotoBOL
        (KChar 'x', []) -> doEdit Z.deleteChar
        (KChar 'X', []) -> doEdit Z.deletePrevChar
        (KChar ':', []) -> (getCommander >=> focus) this >> return True
        _ -> return False
  where
    doEdit f = applyEdit f this >> return True

insertKeyHandler_ :: KeyHandler
insertKeyHandler_ this key mod =
    case (key, mod) of
        (KEsc, []) -> setMode this NormalMode >> return True
        _ -> do
            e <- getEditor this
            handleKeyEvent e key mod

getEditor :: Widget ModalEdit -> IO (Widget Edit)
getEditor = (editor <~~)

getEditor' :: ModalEdit -> IO (Widget Edit)
getEditor' this = return $ editor this

getCommander :: Widget ModalEdit -> IO (Widget CommandEdit)
getCommander = (commander <~~)

keyEventHandler_ this key mod = do
    m <- getState this
    case mode m of
        NormalMode -> normalKeyHandler m this key mod
        InsertMode -> insertKeyHandler m this key mod

modalEditWidget cmd = do
    e <- multiLineEditWidget
    let initSt = ModalEdit { normalKeyHandler = normalKeyHandler_
                           , insertKeyHandler = insertKeyHandler_
                           , mode = NormalMode
                           , editor = e
                           , commander = cmd
                           }
    w <- newWidget initSt $ \w ->
        w { growHorizontal_ = const $ growHorizontal e
          , growVertical_ = const $ growVertical e
          , setCurrentPosition_ = \_ pos -> setCurrentPosition e pos
          , getCursorPosition_ = const $ getCursorPosition e
          , render_ = \_ d r -> render e d r
          , keyEventHandler = keyEventHandler_
          }
    w `onGainFocus` const (focus e)
    w `onLoseFocus` const (unfocus e)
    return w
