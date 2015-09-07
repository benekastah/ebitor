module Ebitor.Language
    ( parseKeyEvents
    ) where

import Control.Monad
import Data.Char
import Data.Foldable (foldr', foldr1)
import Data.List (nub)
import Data.Maybe

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.ByteString.Lazy
import qualified Data.ByteString.Lazy as B

import Ebitor.Events

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

backTab :: Parser Key
backTab = do
    istring "BackTab"
    return KBackTab

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
    c <- try (escaped $ char '<') <|> try (escaped $ char '\\') <|> noneOf "<"
    return $ KChar c
  where
    escaped c = do { char '\\' ; c }

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

modifiedKeyCombo :: Parser Event
modifiedKeyCombo = do
    mods <- many1 $ try modifier'
    key <- tryChoice [specialKey, charKey]
    return $ EvKey key $ nub mods
  where
    modifier' = do { m <- modifier ; char '-' ; return m }

unmodifiedKeyCombo :: Parser Event
unmodifiedKeyCombo = do
    key <- tryChoice [bracketed specialKey, charKey]
    return $ EvKey key []

keyCombo :: Parser Event
keyCombo = tryChoice [bracketed modifiedKeyCombo, unmodifiedKeyCombo]

parseKeyEvents :: B.ByteString -> Either ParseError [Event]
parseKeyEvents = parse (many1 keyCombo) "Key combos"
