{-# LANGUAGE OverloadedStrings #-}
module Commander
    ( parseCommand
    ) where

import Control.Monad

import Data.Text
-- import Graphics.Vty
import Graphics.Vty.Widgets.All
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

type Identifier = Text

identifier :: Parser Identifier
identifier = do
    start <- letter
    end <- many (alphaNum <|> oneOf "_-")
    return $ pack (start:end)

commandParser :: Parser [Identifier]
commandParser = do
    ids <- sepBy1 identifier spaces
    eof
    return ids

parseCommand :: Text -> Either ParseError [Identifier]
parseCommand = parse commandParser "(unkown)"
