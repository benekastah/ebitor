{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Command
    ( Action
    , ActionMap
    , CmdSyntaxNode(..)
    , Command(..)
    , Commander()
    , Message(..)
    , commander
    , decodeCommand
    , encodeCommand
    , findCommand
    , getServerCommand
    , parseCommand
    , parseCommand'
    ) where

import Data.Either
import Data.List (foldl')
import Data.Map ((!))
import GHC.Generics
import qualified Data.Map as M
import qualified Data.Text as T

import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Ebitor.Command.Parser
import Ebitor.Events.JSON
import Ebitor.Language
import qualified Ebitor.Window as W

data Message = Message T.Text | ErrorMessage T.Text
             deriving (Generic, Show)
instance FromJSON Message
instance ToJSON Message

data Command = CommandSequence [Command]
             | Disconnect
             | Echo Message
             | EditFile FilePath
             | Search (Maybe T.Text)
             | SearchReverse (Maybe T.Text)
             | SendKeys [Event]
             | SplitWindow W.Orientation (Maybe FilePath)
             | UpdateDisplaySize (Int, Int)
             | WriteFile (Maybe FilePath)
             deriving (Generic, Show)
instance FromJSON Command
instance ToJSON Command
encodeCommand :: Command -> B.ByteString
encodeCommand = encode
decodeCommand :: B.ByteString -> Maybe Command
decodeCommand = decode


type CommandName = T.Text

type CommandMatch = (Bool, [CommandName])

data CommandMatcher = Empty
                    | Node (M.Map Char CommandMatcher) CommandName
                    deriving (Show)

type Action = [CmdSyntaxNode] -> Either T.Text Command
type ActionMap = M.Map CommandName Action

data Commander = Commander
                 { actionMap :: ActionMap
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

findCommand :: Commander -> CmdSyntaxNode -> Either T.Text CmdSyntaxNode
findCommand cmds cmd@(CmdCall partialCommand args) =
    case matchCommands (matcher cmds) partialCommand of
        (True, _) -> Right cmd
        (False, command:[]) -> Right $ CmdCall command args
        (False, []) -> Left $ T.append "No commands found matching " partialCommand
        (False, matches) -> Left $ T.unwords ("Multiple matches found:":matches)
findCommand cmds _ = Left "Can only resolve CmdCall nodes"

getServerCommand :: Commander -> CmdSyntaxNode -> Either T.Text Command
getServerCommand cmds cmd = case findCommand cmds cmd of
    Right (CmdCall command args) -> get command args
    Right _ -> Left $ "Not a command"
    Left m -> Left m
  where
    get = (actionMap cmds !)

commander :: Commander
commander = Commander { actionMap = cmds
                      , matcher = getCommandMatcher $ M.keys cmds }
  where
    cmds = M.fromList
           [ ("e", edit)
           , ("echo", echo)
           , ("echoerr", echoErr)
           , ("edit", edit)
           , ("quit", quit)
           , ("search", search False)
           , ("s", search False)
           , ("search-back", search True)
           , ("send-keys", sendKeys)
           , ("split", split W.Horizontal)
           , ("vsplit", split W.Vertical)
           , ("w", write)
           , ("wq", writeQuit)
           , ("write", write)
           ]

    arityError = Left "Wrong number of arguments"

    sendKeys :: Action
    sendKeys [CmdString keys] = case parseKeyEvents keys of
        Right evs -> Right $ SendKeys evs
        Left e -> Left $ T.pack $ show e
    sendKeys _ = arityError

    edit :: Action
    edit [CmdString fname] = Right $ EditFile $ T.unpack fname
    edit _ = arityError

    write :: Action
    write [CmdString fname] = Right $ WriteFile $ Just $ T.unpack fname
    write [] = Right $ WriteFile Nothing
    write _ = arityError

    writeQuit :: Action
    writeQuit arg = case partitionEithers [write arg, quit []] of
        ([], cmds) -> Right $ CommandSequence cmds
        ((err:_), _) -> Left err

    quit :: Action
    quit [] = Right Disconnect
    quit _ = arityError

    echo :: Action
    echo [CmdString msg] = Right $ Echo $ Message msg
    echo _ = arityError

    echoErr :: Action
    echoErr [CmdString msg] = Right $ Echo $ ErrorMessage msg
    echoErr _ = arityError

    split :: W.Orientation -> Action
    split o [CmdString msg] = Right $ SplitWindow o (Just $ T.unpack msg)
    split o [] = Right $ SplitWindow o Nothing
    split _ _  = arityError

    search :: Bool -> Action
    search rev ls = case ls of
        [CmdString pattern] -> Right $ mkSearch $ Just pattern
        [] -> Right $ mkSearch Nothing
        _ -> arityError
      where
        mkSearch = if rev then SearchReverse else Search
