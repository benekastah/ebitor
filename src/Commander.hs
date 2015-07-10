{-# LANGUAGE OverloadedStrings #-}
module Commander
    ( commandWidget
    ) where

import Control.Monad
import Data.IORef
import Data.List
import Data.Map ((!))
import qualified Data.Map as M

import Data.Text hiding (map, foldl', length)
import Graphics.Vty
import Graphics.Vty.Widgets.All
import qualified Data.Text as T

import Api
import Application hiding (editor)
import CommandParser

editor = commandEditEditor

type DisplayedChild = Either (Widget FormattedText) (Widget Edit)

getDisplayedChild :: Widget CommandEdit -> IO DisplayedChild
getDisplayedChild this = getState this >>= getDisplayedChild'

getDisplayedChild' :: CommandEdit -> IO DisplayedChild
getDisplayedChild' st =
    case displayMode st of
        MessageMode -> return $ Left $ message st
        CommandMode -> return $ Right $ editor st

addCommand :: CommandMatcher -> Command -> CommandMatcher
addCommand matcher command =
    merge matcher $ T.foldr buildMatcher (Node M.empty command) command
  where
    buildMatcher ch matcher = Node (M.singleton ch matcher) ""
    merge Empty b = b
    merge a Empty = a
    merge (Node m c) (Node m2 c2) =
        Node (M.unionWith merge m m2) $ if T.null c then c2 else c

matchCommands :: CommandMatcher -> Command -> [Command]
matchCommands matcher command =
    findMatches [] (T.foldl' findNode (Just matcher) command)
  where
    findNode :: Maybe CommandMatcher -> Char -> Maybe CommandMatcher
    findNode (Just (Node m _)) ch = M.lookup ch m
    findNode (Just Empty) _ = Nothing
    findNode Nothing _ = Nothing

    findMatches :: [Command] -> Maybe CommandMatcher -> [Command]
    findMatches ls (Just (Node m c)) =
        foldl' findMatches (if T.null c then ls else c:ls) (map Just $ M.elems m)
    findMatches ls (Just Empty) = ls
    findMatches ls Nothing = ls

getCommandMatcher :: [Command] -> CommandMatcher
getCommandMatcher = foldl' addCommand Empty


getCommands appRef = Commands { actionMap = cmds, matcher = match }
  where
    cmds = M.fromList
           [ ("quit", quitCommand)
           , ("echo", echoCommand)
           , ("help", helpCommand)
           ]

    match = getCommandMatcher $ M.keys cmds

    echo' = echo appRef

    showText = T.pack . show

    arityEq expected actual =
        if actual == expected then Nothing
        else Just $ T.unwords
            ["Expected", showText expected, "args, got", showText actual]

    arityGt expected actual =
        if actual > expected then Nothing
        else Just $ T.unwords
            ["Expected >", showText expected, "args, got", showText actual]

    arityGte expected actual =
        if actual >= expected then Nothing
        else Just $ T.unwords
            ["Expected >=", showText expected, "args, got", showText actual]

    arityLt expected actual =
        if actual < expected then Nothing
        else Just $ T.unwords
            ["Expected <", showText expected, "args, got", showText actual]

    arityLte expected actual =
        if actual <= expected then Nothing
        else Just $ T.unwords
            ["Expected <=", showText expected, "args, got", showText actual]

    commandFn checkArity fn ls =
        case checkArity (length ls) of
            Nothing -> fn ls
            Just msg -> echo' msg

    quitCommand = commandFn (arityEq 0) $ \_ -> quit appRef

    helpCommand = commandFn (arityEq 0) $ \_ ->
        echo' $ T.unwords ("Available commands are:":M.keys cmds)

    echoCommand = commandFn (arityGte 1) $ echo' . T.unwords


runCommand echo' cmds (partialCommand:args) =
    case matchCommands (matcher cmds) partialCommand of
        command:[] -> (actionMap cmds ! command) args
        [] -> echo' $ append "No commands found matching " partialCommand
        matches -> echo' $ T.unwords ("Multiple matches found:":matches)

commandWidget appRef = do
    msg <- plainText "Welcome to ebitor!"
    e <- editWidget
    let initSt = CommandEdit
                    { commandEditEditor = e
                    , message = msg
                    , displayMode = MessageMode
                    , commands = getCommands appRef
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
                    (KEsc, []) -> do
                        app <- readIORef appRef
                        focusPrevious $ focusGroup app
                        return True
                    _ -> handleKeyEvent e key mod
          }

    e `onActivate` \_ -> do
        let echo' = echo appRef
        text <- getEditText e
        cmds <- commands <~~ widget
        case parseCommand text of
            Left err -> echo' $ pack (show err)
            Right ids -> runCommand echo' cmds ids

    widget `onGainFocus` \this -> do
        setText msg ""
        updateWidgetState this $ \st -> st {displayMode = CommandMode}
        focus e

    widget `onLoseFocus` \this -> do
        setEditText e ""
        unfocus e
        updateWidgetState this $ \st -> st {displayMode = MessageMode}

    return widget
