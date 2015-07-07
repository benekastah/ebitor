{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.IORef

import Application
import Commander
import ModalEditor

import Graphics.Vty
import Graphics.Vty.Widgets.All

getApplication :: IO (Application ModalEdit CommandEdit)
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

    fg `onKeyPressed` \_ k mod ->
        case (k, mod) of
            (KChar 'q', [MCtrl]) -> shutdownUi >> return True
            _ -> return False

    writeIORef appRef Application { editor = mainEditor
                                  , commander = comm
                                  , focusGroup = fg
                                  , collection = coll
                                  }
    readIORef appRef

main :: IO ()
main = do
    app <- getApplication
    runUi (collection app) $ defaultContext { focusAttr = fgColor yellow }
