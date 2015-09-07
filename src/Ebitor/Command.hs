{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Command
    ( Command(..)
    , Response(..)
    , decodeCommand
    , decodeResponse
    , encodeResponse
    ) where

import Control.Applicative (pure)
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

type ActionMap = M.Map CommandName ([Command] -> IO ())

data Commands = Commands
                { actionMap :: ActionMap
                , matcher :: CommandMatcher
                }
                deriving (Show)

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

resolveCommand :: Commands -> Command -> Either T.Text Command
resolveCommand cmds cmd@(CmdCall partialCommand args) =
    case matchCommands (matcher cmds) partialCommand of
        (True, _) -> Right cmd
        (False, command:[]) -> Right $ CmdCall command args
        (False, []) -> Left $ T.append "No commands found matching " partialCommand
        (False, matches) -> Left $ T.unwords ("Multiple matches found:":matches)
resolveCommand cmds _ = Left "Can only resolve CmdCall nodes"

runCommand :: Commands -> Command -> Either T.Text (IO ())
runCommand cmds cmd = case resolveCommand cmds cmd of
    Right (CmdCall command args) -> Right $ run command args
    Right _ -> Left $ "Can only run CmdCall nodes"
    Left m -> Left m
  where
    run :: T.Text -> [Command] -> IO ()
    run = actionMap cmds !

newCommands :: ActionMap -> Commands
newCommands cmds = Commands { actionMap = cmds
                            , matcher = getCommandMatcher $ M.keys cmds }


data Response = Screen Editor
              | InvalidCommand
              deriving (Generic, Show)
instance FromJSON Response
instance ToJSON Response


decodeCommand :: B.ByteString -> Maybe Command
decodeCommand = decode

encodeResponse :: Response -> B.ByteString
encodeResponse = encode
decodeResponse :: B.ByteString -> Maybe Response
decodeResponse = decode
