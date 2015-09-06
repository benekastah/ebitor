{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Command
    ( Command(..)
    , Response(..)
    , decodeCommand
    , decodeResponse
    , encodeCommand
    , encodeResponse
    ) where

import Control.Applicative (pure)
import Control.Monad (mzero)
import Data.Aeson
import GHC.Generics
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Ebitor.Edit
import Ebitor.Rope.Cursor (Cursor())
import Ebitor.Rope.Part (RopePart)
import qualified Ebitor.Rope.Generic as RG

instance FromJSON Cursor
instance ToJSON Cursor

instance RopePart a => FromJSON (RG.GenericRope a) where
    parseJSON = withText "String" $ pure . RG.pack . T.unpack

instance RopePart a => ToJSON (RG.GenericRope a) where
    toJSON r = toJSON $ RG.unpack r

data Command = InsertChar Char
             | InsertNewline
             | Backspace
             deriving (Generic, Show)

instance FromJSON Command
instance ToJSON Command

data Response = Screen Editor
              | InvalidCommand
              deriving (Generic, Show)

instance FromJSON Editor
instance ToJSON Editor
instance FromJSON Response
instance ToJSON Response

encodeCommand :: Command -> B.ByteString
encodeCommand = encode
decodeCommand :: B.ByteString -> Maybe Command
decodeCommand = decode

encodeResponse :: Response -> B.ByteString
encodeResponse = encode
decodeResponse :: B.ByteString -> Maybe Response
decodeResponse = decode
