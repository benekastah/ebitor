module Ebitor.CommandParser
    ( Command(..)
    , parseCommand
    , syntaxNodeToText
    ) where

import Control.Applicative (pure)
import Control.Monad (mzero)
import Data.Char

import Data.Aeson (ToJSON, FromJSON)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.ByteString.Lazy
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.Encoding as T

data Command = CmdIdentifier T.Text
                   | CmdNumber Double
                   | CmdString T.Text
                   | CmdCall T.Text [Command]
                   deriving (Show)

syntaxNodeToText (CmdIdentifier t) = t
syntaxNodeToText (CmdString t) = t
syntaxNodeToText (CmdNumber n) =
    T.pack (if n == fromInteger floorN then show floorN else show n)
  where
    floorN = floor n

identifier :: Parser Command
identifier = do
    start <- letter
    end <- many (alphaNum <|> oneOf "_-")
    return $ CmdIdentifier $ T.pack (start:end)

-- Numbers
int :: Parser Command
int = do
    n <- many1 digit
    return $ CmdNumber $ read n

dec :: Parser Command
dec = do
    start <- many digit
    d <- char '.'
    end <- many1 digit
    return $ CmdNumber (read $ '0':(start ++ (d:end)))

num' :: Parser Command
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

bareString :: Parser Command
bareString = do
    t <- many1 $ escapedChar <|> nonSpace
    return $ CmdString $ T.pack t

str = bareString

-- General
expression = num <|> str

commandParser :: Parser Command
commandParser = do
    CmdIdentifier id <- identifier
    spaces
    args <- sepBy expression spaces
    eof
    return $ CmdCall id args

parseCommand :: T.Text -> Either ParseError Command
parseCommand = parse commandParser "User command"

instance FromJSON Command where
    parseJSON = withText "String" doParse
      where
        doParse s = case parseCommand $ T.encodeUtf8 $ T.fromStrict s of
            Right cmd -> pure cmd
            Left e -> mzero
