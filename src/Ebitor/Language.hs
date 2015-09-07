module Ebitor.Language where

import Control.Monad
import Data.Char
import Data.Foldable (foldr', foldr1)
import Data.List (nub)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as B

-- | Representations of non-modifier keys.
--
-- * KFun is indexed from 0 to 63.
--
-- * KUpLeft, KUpRight, KDownLeft, KDownRight, KCenter support varies by
-- keyboard.
--
-- * Actually, support for most of these but KEsc, KChar, KBS, and KEnter vary
-- by keyboard.
--
-- Stolen from https://github.com/coreyoconnor/vty/blob/master/src/Graphics/Vty/Input/Events.hs
data Key = KEsc  | KChar Char | KBS | KEnter
         | KLeft | KRight | KUp | KDown
         | KUpLeft | KUpRight | KDownLeft | KDownRight | KCenter
         | KFun Int | KBackTab | KPrtScr | KPause | KIns
         | KHome | KPageUp | KDel | KEnd | KPageDown | KBegin | KMenu
    deriving (Eq, Show, Read, Ord)

-- | Modifier keys.
--
-- Stolen from https://github.com/coreyoconnor/vty/blob/master/src/Graphics/Vty/Input/Events.hs
data Modifier = MShift | MCtrl | MMeta | MAlt
    deriving (Eq, Show, Read, Ord)

type KeyCombo = (Key, [Modifier])

ichar :: Char -> Parser Char
ichar c = char (toLower c) <|> char (toUpper c)

istring :: String -> Parser String
istring s = liftM reverse $ foldr' collect (return "") $ reverse $ map ichar s
    where collect a b = do { s <- b ; liftM (:s) a }

tryChoice [] = choice []
tryChoice (x:[]) = choice [x]
tryChoice ls = choice (map try $ init ls) <|> last ls

choiceIstrings = tryChoice . map istring

bracketed :: Parser a -> Parser a
bracketed = between (char '<') (char '>')

escape :: Parser Key
escape = do
    choiceIstrings ["Escape", "Esc"]
    return KEsc

enter :: Parser Key
enter = do
    choiceIstrings ["Enter", "Return", "CR"]
    return KEnter

backspace :: Parser Key
backspace = do
    choiceIstrings ["Backspace", "BS"]
    return KBS

up :: Parser Key
up = do
    istring "Up"
    return KUp

down :: Parser Key
down = do
    istring "Down"
    return KDown

left :: Parser Key
left = do
    istring "Left"
    return KLeft

right :: Parser Key
right = do
    istring "Right"
    return KRight

upLeft :: Parser Key
upLeft = do
    istring "UpLeft"
    return KUpLeft

upRight :: Parser Key
upRight = do
    istring "UpRight"
    return KUpRight

downLeft :: Parser Key
downLeft = do
    istring "DownLeft"
    return KDownLeft

downRight :: Parser Key
downRight = do
    istring "DownRight"
    return KDownRight

center :: Parser Key
center = do
    istring "Center"
    return KCenter

function :: Parser Key
function = do
    ichar 'F'
    d <- many1 digit
    return $ KFun $ read d

home :: Parser Key
home = do
    istring "Home"
    return KHome

pageUp :: Parser Key
pageUp = do
    istring "PageUp"
    return KPageUp

pageDown :: Parser Key
pageDown = do
    istring "PageDown"
    return KPageDown

delete :: Parser Key
delete = do
    choiceIstrings ["Delete", "Del"]
    return KDel

begin :: Parser Key
begin = do
    istring "Begin"
    return KBegin

end :: Parser Key
end = do
    istring "End"
    return KEnd

prtScr :: Parser Key
prtScr = do
    istring "PrtScr"
    return KPrtScr

pause :: Parser Key
pause = do
    istring "Pause"
    return KPause

insert :: Parser Key
insert = do
    choiceIstrings ["Insert", "Ins"]
    return KIns

menu :: Parser Key
menu = do
    istring "Menu"
    return KMenu

charKey :: Parser Key
charKey = do
    angle <- optionMaybe $ string "\\<"
    case angle of
        Just _ -> return $ KChar '<'
        Nothing -> do
            c <- anyChar
            return $ KChar c

specialKey :: Parser Key
specialKey = tryChoice [ escape, enter, backspace, up, down, left, right, upLeft
                       , upRight, downLeft, downRight, center, function, home
                       , pageUp, pageDown, delete, begin, end, prtScr, pause
                       , insert, menu ]

shift :: Parser Modifier
shift = do
    choiceIstrings ["Shift", "S"]
    return MShift

control :: Parser Modifier
control = do
    choiceIstrings ["Control", "Ctrl", "C"]
    return MCtrl

meta :: Parser Modifier
meta = do
    choiceIstrings ["Meta", "M"]
    return MMeta

alt :: Parser Modifier
alt = do
    choiceIstrings ["Alt", "A"]
    return MAlt

modifier :: Parser Modifier
modifier = tryChoice [shift, control, meta, alt]

keyToKeyCombo :: Key -> KeyCombo
keyToKeyCombo (KChar c) | isUpper c = (KChar c, [MShift])
keyToKeyCombo k = (k, [])

modifiedKeyCombo :: Parser KeyCombo
modifiedKeyCombo = do
    mods <- many1 $ try modifier'
    key <- tryChoice [specialKey, charKey]
    let (key', mods') = keyToKeyCombo key
    return (key', nub (mods ++ mods'))
  where
    modifier' = do { m <- modifier ; char '-' ; return m }


unmodifiedKeyCombo :: Parser KeyCombo
unmodifiedKeyCombo = do
    key <- tryChoice [bracketed specialKey, charKey]
    return $ keyToKeyCombo key

keyCombo :: Parser KeyCombo
keyCombo = tryChoice [bracketed modifiedKeyCombo, unmodifiedKeyCombo]

parseKeyCombos :: B.ByteString -> Either ParseError [KeyCombo]
parseKeyCombos = parse (many1 keyCombo) "Key combos"
