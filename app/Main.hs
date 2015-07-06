{-# LANGUAGE OverloadedStrings #-}
module Main where

import ModalEditor
import Commander

import Data.Text
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All hiding (applyEdit)


main :: IO ()
main = do
    fg <- newFocusGroup
    commander <- commandWidget fg
    mainEditor <- modalEditWidget commander

    _ <- addToFocusGroup fg mainEditor
    _ <- addToFocusGroup fg commander
    _ <- setFocusGroupNextKey fg (KChar 'w') [MCtrl]
    _ <- setFocusGroupPrevKey fg (KChar 'w') [MShift, MCtrl]

    ui <- return mainEditor <--> hBorder <--> return commander
    c <- centered ui

    coll <- newCollection
    _ <- addToCollection coll c fg

    fg `onKeyPressed` \_ k mod ->
        case (k, mod) of
            (KChar 'q', [MCtrl]) -> shutdownUi >> return True
            _ -> return False

    runUi coll $ defaultContext { focusAttr = fgColor yellow }
