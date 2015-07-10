{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef

import qualified Api
import Application
import Commander
import Lua
import ModalEditor

import Graphics.Vty
import Graphics.Vty.Widgets.All

getApplication :: IO (IORef Application)
getApplication = do
    appRef <- newIORef EmptyApplication
    comm <- commandWidget appRef
    mainEditor <- modalEditWidget appRef

    fg <- newFocusGroup
    _ <- addToFocusGroup fg mainEditor
    _ <- addToFocusGroup fg comm
    _ <- setFocusGroupNextKey fg (KChar 'w') [MCtrl]
    _ <- setFocusGroupPrevKey fg (KChar 'w') [MShift, MCtrl]

    ui <- return mainEditor <--> hBorder <--> return comm
    c <- centered ui

    coll <- newCollection
    _ <- addToCollection coll c fg

    l <- newLuaState appRef

    writeIORef appRef Application { editor = mainEditor
                                  , commander = comm
                                  , focusGroup = fg
                                  , collection = coll
                                  , luaState = l
                                  }
    return appRef

main :: IO ()
main = do
    appRef <- getApplication
    app <- readIORef appRef
    schedule $ runConfig appRef Nothing
    runUi (collection app) $ defaultContext { focusAttr = fgColor yellow }
