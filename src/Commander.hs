{-# LANGUAGE OverloadedStrings #-}
module Commander
    ( commandWidget
    , CommandEdit
    ) where

import Control.Monad

import Data.Text
import Graphics.Vty
import Graphics.Vty.Widgets.All

import CommandParser

data DisplayMode = MessageMode | CommandMode
                   deriving (Show)

data CommandEdit = CommandEdit
                    { editor :: Widget Edit
                    , message :: Widget FormattedText
                    , mode :: DisplayMode
                    }

type DisplayedChild = Either (Widget FormattedText) (Widget Edit)

instance Show CommandEdit where
    show e = "CommandEdit " ++ show (mode e)

getDisplayedChild :: Widget CommandEdit -> IO DisplayedChild
getDisplayedChild this = getState this >>= getDisplayedChild'

getDisplayedChild' :: CommandEdit -> IO DisplayedChild
getDisplayedChild' st =
    case mode st of
        MessageMode -> return $ Left $ message st
        CommandMode -> return $ Right $ editor st

commandWidget fg = do
    msg <- plainText "Welcome to ebitor!"
    e <- editWidget
    let initSt = CommandEdit
                    { editor = e
                    , message = msg
                    , mode = MessageMode
                    }

    widget <- newWidget initSt $ \w ->
        w { growHorizontal_ = \st -> do
                child <- getDisplayedChild' st
                case child of
                    Left e -> growHorizontal e
                    Right e -> growHorizontal e
          , growVertical_ = \st -> do
                child <- getDisplayedChild' st
                case child of
                    Left e -> growVertical e
                    Right e -> growVertical e
          , setCurrentPosition_ = \this pos -> do
                e <- editor <~~ this
                m <- message <~~ this
                setCurrentPosition e pos
                setCurrentPosition m pos
          , getCursorPosition_ = const $ getCursorPosition e
          , render_ = \this d r -> do
                child <- getDisplayedChild this
                case child of
                    Left e -> render e d r
                    Right e -> render e d r
          , keyEventHandler = \this key mod ->
                case (key, mod) of
                    (KEsc, []) -> focusPrevious fg >> return True
                    _ -> handleKeyEvent e key mod
          }

    e `onActivate` \_ -> do
        let showMessage m = do
                            setText msg m
                            -- should show the message
                            focusPrevious fg
        text <- getEditText e
        case parseCommand text of
            Left err -> showMessage $ pack (show err)
            Right ids -> case ids of
                ["q"] -> shutdownUi
                ["echo", x] -> showMessage x
                _ -> showMessage $ append "Unable to run command: " text

    widget `onGainFocus` \this -> do
        setText msg ""
        updateWidgetState this $ \st -> st {mode = CommandMode}
        focus e

    widget `onLoseFocus` \this -> do
        setEditText e ""
        unfocus e
        updateWidgetState this $ \st -> st {mode = MessageMode}

    return widget
