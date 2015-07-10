{-# LANGUAGE OverloadedStrings #-}
module Lua
    ( runConfig
    , newLuaState
    ) where

import Control.Monad
import Data.IORef
import System.FilePath
import System.Directory

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Scripting.Lua as Lua

import Application
import Api

newLuaState appRef = do
    l <- Lua.newstate
    Lua.openlibs l

    Lua.registerhsfunction l "quit" (quit appRef)
    Lua.registerhsfunction l "echo" (echo appRef . T.decodeUtf8)

    return l

runConfig :: IORef Application -> Maybe FilePath -> IO ()
runConfig appRef Nothing = runDefaultConfig appRef
runConfig appRef (Just fp) = do
    l <- liftM luaState (readIORef appRef)
    Lua.loadfile l fp
    Lua.call l 0 0

runDefaultConfig :: IORef Application -> IO ()
runDefaultConfig appRef = do
    home <- getHomeDirectory
    fp <- findFile [home] ".ebitor.lua"
    case fp of
        Just _ -> runConfig appRef fp
        Nothing -> return ()
