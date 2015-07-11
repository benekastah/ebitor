{-# LANGUAGE OverloadedStrings #-}
module Api
    ( quit
    , echo
    ) where

import Control.Monad
import Data.IORef
import System.Directory

import Graphics.Vty.Widgets.All
import qualified Scripting.Lua as Lua

import Application

quit appRef = do
    l <- luaState <~ appRef
    Lua.close l
    shutdownUi

echo appRef m = do
    app <- readIORef appRef
    let cmdr = commander app
    msg <- message <~~ cmdr
    setText msg m
    updateWidgetState cmdr $ \st -> st {displayMode = MessageMode}
    isFocused <- focused <~ cmdr
    when isFocused (focusPrevious $ focusGroup app)
