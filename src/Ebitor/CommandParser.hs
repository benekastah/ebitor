module Ebitor.CommandParser
    ( parseCommand
    , syntaxNodeToText
    ) where

import Data.Char

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text
import qualified Data.Text as T
import qualified Data.Text.Read as T

import Ebitor.Application

syntaxNodeToText (CmdIdentifier t) = t
syntaxNodeToText (CmdString t) = t
syntaxNodeToText (CmdNumber n) =
    T.pack (if n == fromInteger floorN then show floorN else show n)
  where
    floorN = floor n

identifier :: Parser CmdSyntaxNode
identifier = do
    start <- letter
    end <- many (alphaNum <|> oneOf "_-")
    return $ CmdIdentifier $ T.pack (start:end)

-- Numbers
int :: Parser CmdSyntaxNode
int = do
    n <- many1 digit
    return $ CmdNumber $ read n

dec :: Parser CmdSyntaxNode
dec = do
    start <- many digit
    d <- char '.'
    end <- many1 digit
    return $ CmdNumber (read $ '0':(start ++ (d:end)))

num' :: Parser CmdSyntaxNode
num' = try dec <|> int

negNum = do
    char '-'
    CmdNumber n <- num'
    return $ CmdNumber (-n)

num = try negNum <|> num'

-- Strings
nonSpace = satisfy $ not . isSpace
escapedChar = do
    char '\\'
    space <|> char '\\'

bareString :: Parser CmdSyntaxNode
bareString = do
    t <- many1 $ escapedChar <|> nonSpace
    return $ CmdString $ T.pack t

str = bareString

-- General
expression = num <|> str

commandParser :: Parser CmdSyntaxNode
commandParser = do
    CmdIdentifier id <- identifier
    spaces
    args <- sepBy expression spaces
    eof
    return $ CmdCall id args

parseCommand :: T.Text -> Either ParseError CmdSyntaxNode
parseCommand = parse commandParser "User command"
