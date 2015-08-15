module Ebitor.Api
    ( quit
    , echo
    , edit
    , write
    ) where

import Control.Monad
import Data.IORef
import System.Directory
import System.FilePath

import Graphics.Vty.Widgets.All
import qualified Scripting.Lua as Lua
import qualified Data.Text as T

import Ebitor.Application
import Ebitor.ModalEditor as ModalEditor

quit :: IORef Application -> IO ()
quit appRef = do
    l <- luaState <~ appRef
    Lua.close l
    shutdownUi

echo :: IORef Application -> T.Text -> IO ()
echo appRef m = do
    app <- readIORef appRef
    let cmdr = commander app
    msg <- message <~~ cmdr
    setText msg m
    updateWidgetState cmdr $ \st -> st {displayMode = MessageMode}
    isFocused <- focused <~ cmdr
    when isFocused (focusPrevious $ focusGroup app)

edit :: IORef Application -> Maybe FilePath -> IO ()
edit appRef Nothing = do
    e <- editor <~ appRef
    mfpath <- filePath <~~ e
    case mfpath of
        Just fpath -> edit appRef mfpath
        _ -> echo appRef "No file name"

edit appRef (Just fpath) = do
    s <- readFile fpath
    e <- editor <~ appRef
    updateWidgetState e $ \st -> st {filePath = Just fpath}
    ModalEditor.setEditText e $ T.pack s

write :: IORef Application -> Maybe FilePath -> IO ()
write appRef Nothing = do
    e <- editor <~ appRef
    mfpath <- filePath <~~ e
    case mfpath of
        Just fpath -> write appRef mfpath
        _ -> echo appRef "No file name"

write appRef (Just fpath) = do
    e <- editor <~ appRef
    updateWidgetState e $ \st -> st {filePath = Just fpath}
    content <- ModalEditor.getEditText e
    writeFile fpath (T.unpack content)
