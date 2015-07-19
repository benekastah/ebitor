module Ebitor.Application
    ( Application(..)
    , Command
    , CommandEdit(..)
    , CommandMatch
    , CommandMatcher(..)
    , Commands(..)
    , DisplayMode(..)
    , EditMode(..)
    , ModalEdit(..)
    , KeyHandler
    , CmdSyntaxNode(..)
    ) where

import Data.IORef
import System.FilePath

import Graphics.Vty
import Graphics.Vty.Widgets.All
import Scripting.Lua
import qualified Data.Map as M
import qualified Data.Text as T

type KeyHandler = Widget ModalEdit -> Key -> [Modifier] -> IO Bool

data EditMode = NormalMode | InsertMode
                deriving (Show)

data ModalEdit = ModalEdit
                { normalKeyHandler :: KeyHandler
                , insertKeyHandler :: KeyHandler
                , editMode :: EditMode
                , modalEditEditor :: Widget Edit
                , application :: IORef Application
                , filePath :: Maybe FilePath
                }

instance Show ModalEdit where
    show e = "ModalEdit " ++ show (editMode e)

data DisplayMode = MessageMode | CommandMode
                   deriving (Show)

data CmdSyntaxNode = CmdIdentifier T.Text
                   | CmdNumber Double
                   | CmdString T.Text
                   | CmdCall T.Text [CmdSyntaxNode]
                   deriving (Show)

type Command = T.Text

type CommandMatch = (Bool, [Command])

data CommandMatcher = Empty
                    | Node (M.Map Char CommandMatcher) Command

data Commands = Commands
                { actionMap :: M.Map Command ([CmdSyntaxNode] -> IO ())
                , matcher :: CommandMatcher
                }

data CommandEdit = CommandEdit
                   { commandEditEditor :: Widget Edit
                   , message :: Widget FormattedText
                   , displayMode :: DisplayMode
                   , commands :: Commands
                   }

instance Show CommandEdit where
    show e = "CommandEdit " ++ show (displayMode e)

data Application = EmptyApplication
                 | Application
                   { editor :: Widget ModalEdit
                   , commander :: Widget CommandEdit
                   , focusGroup :: Widget FocusGroup
                   , collection :: Collection
                   , luaState :: LuaState
                   }
