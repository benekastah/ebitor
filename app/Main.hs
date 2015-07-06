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
    commandMessage <- plainText ""
    commandEditor <- editWidget
    mainEditor <- modalEditWidget commandEditor

    _ <- addToFocusGroup fg mainEditor
    _ <- addToFocusGroup fg commandEditor
    _ <- setFocusGroupNextKey fg (KChar 'w') [MCtrl]
    _ <- setFocusGroupPrevKey fg (KChar 'w') [MShift, MCtrl]

    ui <- return mainEditor <--> hBorder <--> return commandEditor
    c <- centered ui

    coll <- newCollection
    _ <- addToCollection coll c fg

    commandEditor `onActivate` \this -> do
        let showMessage = setText commandMessage
        text <- getEditText this
        case parseCommand text of
            Left err -> showMessage $ pack (show err)
            Right ids -> case ids of
                ["q"] -> shutdownUi
                ["echo", x] -> showMessage x
                _ -> showMessage $ append "Unable to run command: " text

    fg `onKeyPressed` \_ k mod ->
        case (k, mod) of
            (KEsc, []) -> do
                commandEditorFocused <- focused <~ commandEditor
                if commandEditorFocused then do
                    setEditText commandEditor ""
                    focusPrevious fg
                    return True
                else
                    return False
            (KChar 'q', [MCtrl]) -> shutdownUi >> return True
            _ -> return False

    runUi coll $ defaultContext { focusAttr = fgColor yellow }
