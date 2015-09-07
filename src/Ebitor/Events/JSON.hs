{-# LANGUAGE FlexibleInstances #-}
module Ebitor.Events.JSON
    ( Key(..)
    , Modifier(..)
    , Button(..)
    , Event(..)
    ) where

import Control.Applicative (pure)
import Control.Monad (mzero)
import Data.List (foldl')

import Data.Aeson
import Data.List
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

import Ebitor.Events
import Ebitor.Language

keyToString :: Key -> String
keyToString KEsc = "Esc"
keyToString (KChar '<') = "\\<"
keyToString (KChar '\\') = "\\\\"
keyToString (KChar c) = [c]
keyToString KBS = "BS"
keyToString KEnter = "Enter"
keyToString KLeft = "Left"
keyToString KRight = "Right"
keyToString KUp = "Up"
keyToString KDown = "Down"
keyToString KUpLeft = "UpLeft"
keyToString KUpRight = "UpRight"
keyToString KDownLeft = "DownLeft"
keyToString KDownRight = "DownRight"
keyToString KCenter = "Center"
keyToString (KFun i) = "F" ++ show i
keyToString KBackTab = "BackTab"
keyToString KPrtScr = "PrtScr"
keyToString KPause = "Pause"
keyToString KIns = "Ins"
keyToString KHome = "Home"
keyToString KPageUp = "PageUp"
keyToString KDel = "Del"
keyToString KEnd = "End"
keyToString KPageDown = "PageDown"
keyToString KBegin = "Begin"
keyToString KMenu = "Menu"

modifierToString :: Modifier -> String
modifierToString MShift = "S"
modifierToString MCtrl = "C"
modifierToString MMeta = "M"
modifierToString MAlt = "A"

eventToString :: Event -> String
eventToString (EvKey k@(KChar _) []) = keyToString k
eventToString (EvKey k []) = "<" ++ keyToString k ++ ">"
eventToString (EvKey k mods) =
    let m = intercalate "-" $ map modifierToString mods
    in  "<" ++ m ++ "-" ++ keyToString k ++ ">"

instance FromJSON [Event] where
    parseJSON = withText "String" doParse
      where
        doParse s = case parseKeyEvents $ T.encodeUtf8 $ T.fromStrict s of
            Right evs -> pure evs
            Left e -> mzero

instance ToJSON [Event] where
    toJSON evs = toJSON $ foldl' (++) "" $ map eventToString evs
