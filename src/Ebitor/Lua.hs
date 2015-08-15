module Ebitor.Lua
    ( runConfig
    , newLuaState
    ) where

import Control.Monad
import Data.IORef
import Data.Maybe
import System.Directory
import System.FilePath

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Scripting.Lua as Lua
import Graphics.Vty.Widgets.All ((<~))

import Ebitor.Application
import Ebitor.Api

newLuaState appRef = do
    l <- Lua.newstate
    Lua.openlibs l

    -- Set up editor functions
    Lua.registerhsfunction l "quit" (quit appRef)
    Lua.registerrawhsfunction l "print" (luaPrint appRef)

    return l

luaPrint appRef l = do
    nargs <- Lua.gettop l
    args <- sequence [Lua.tostring l n | n <- [1..nargs]]
    echo appRef $ T.unwords (map T.decodeUtf8 args)
    return 0

echoLuaErr appRef = do
    l <- luaState <~ appRef
    msg <- Lua.tostring l (-1)
    echo appRef $ T.decodeUtf8 msg

runConfig :: IORef Application -> Maybe FilePath -> IO ()
runConfig appRef Nothing = runDefaultConfig appRef
runConfig appRef (Just fp) = do
    l <- luaState <~ appRef
    r <- Lua.loadfile l fp
    if r == 0
        then Lua.call l 0 0
        else echoLuaErr appRef

runDefaultConfig :: IORef Application -> IO ()
runDefaultConfig appRef = do
    home <- getHomeDirectory
    fp <- findFile [home] ".ebitor.lua"
    case fp of
        Just _ -> runConfig appRef fp
        Nothing -> return ()
