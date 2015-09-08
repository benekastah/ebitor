module Ebitor.Command
    ( Action
    , ActionMap
    , CmdSyntaxNode(..)
    , Commands(..)
    , newCommands
    , parseCommand
    , parseCommand'
    , resolveCommand
    , runCommand
    ) where

import Control.Applicative (pure)
import Control.Exception
import Control.Monad (mzero)
import Data.Aeson
import Data.List
import Data.Map ((!))
import GHC.Generics
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as M
import qualified Data.Text as T

import Ebitor.Command.Parser
import Ebitor.Edit
import Ebitor.Events
import Ebitor.Events.JSON
import Ebitor.Language
import Ebitor.Rope.Cursor (Cursor())
import Ebitor.Rope.Part (RopePart)
import qualified Ebitor.Rope.Generic as RG

type CommandName = T.Text

type CommandMatch = (Bool, [CommandName])

data CommandMatcher = Empty
                    | Node (M.Map Char CommandMatcher) CommandName
                    deriving (Show)

type Action a = [CmdSyntaxNode] -> IO (Either T.Text a)
type ActionMap a = M.Map CommandName (Action a)

data Commands a = Commands
                  { actionMap :: ActionMap a
                  , matcher :: CommandMatcher
                  }

addCommand :: CommandMatcher -> CommandName -> CommandMatcher
addCommand matcher command =
    merge matcher $ T.foldr buildMatcher (Node M.empty command) command
  where
    buildMatcher ch matcher = Node (M.singleton ch matcher) ""
    merge Empty b = b
    merge a Empty = a
    merge (Node m c) (Node m2 c2) =
        Node (M.unionWith merge m m2) $ if T.null c then c2 else c

matchCommands :: CommandMatcher -> CommandName -> CommandMatch
matchCommands matcher command =
    findMatches (False, []) (T.foldl' findNode (Just matcher) command)
  where
    findNode :: Maybe CommandMatcher -> Char -> Maybe CommandMatcher
    findNode (Just (Node m _)) ch = M.lookup ch m
    findNode (Just Empty) _ = Nothing
    findNode Nothing _ = Nothing

    findMatches :: CommandMatch -> Maybe CommandMatcher -> CommandMatch
    findMatches (match, ls) (Just (Node m c)) =
        let ls' = if T.null c then ls else c:ls
            match' = match || c == command
        in foldl' findMatches (match', ls') (map Just $ M.elems m)
    findMatches match (Just Empty) = findMatches match Nothing
    findMatches match Nothing = match

getCommandMatcher :: [CommandName] -> CommandMatcher
getCommandMatcher = foldl' addCommand Empty

resolveCommand :: Commands a -> CmdSyntaxNode -> Either T.Text CmdSyntaxNode
resolveCommand cmds cmd@(CmdCall partialCommand args) =
    case matchCommands (matcher cmds) partialCommand of
        (True, _) -> Right cmd
        (False, command:[]) -> Right $ CmdCall command args
        (False, []) -> Left $ T.append "No commands found matching " partialCommand
        (False, matches) -> Left $ T.unwords ("Multiple matches found:":matches)
resolveCommand cmds _ = Left "Can only resolve CmdCall nodes"

runCommand :: Commands a -> CmdSyntaxNode -> IO (Either T.Text a)
runCommand cmds cmd = case resolveCommand cmds cmd of
    Right (CmdCall command args) -> run command args
    Right _ -> return $ Left $ "Can only run CmdCall nodes"
    Left m -> return $ Left m
  where
    run = (actionMap cmds !)

newCommands :: ActionMap a -> Commands a
newCommands cmds = Commands { actionMap = cmds
                            , matcher = getCommandMatcher $ M.keys cmds }
