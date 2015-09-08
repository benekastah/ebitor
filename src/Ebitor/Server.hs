{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Server
    ( Command(..)
    , Response(..)
    , decodeCommand
    , decodeResponse
    , encodeCommand
    , encodeResponse
    , runServer
    , runServerThread
    , defaultSockAddr
    ) where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.Fix (fix)
import Data.IORef
import Data.Maybe
import GHC.Generics
import GHC.Int (Int64)
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString.Lazy (recv, send)
import System.IO
import qualified Data.Map as M

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Ebitor.Edit
import Ebitor.Events.JSON
import Ebitor.Language
import qualified Ebitor.Rope as R

type Msg = (Int, B.ByteString)

data Command = SendKeys [Event]
             | EditFile FilePath
             deriving (Generic, Show)
instance FromJSON Command
instance ToJSON Command
encodeCommand :: Command -> B.ByteString
encodeCommand = encode
decodeCommand :: B.ByteString -> Maybe Command
decodeCommand = decode


data Response = Screen Editor
              | InvalidCommand
              deriving (Generic, Show)
instance FromJSON Response
instance ToJSON Response
encodeResponse :: Response -> B.ByteString
encodeResponse = encode
decodeResponse :: B.ByteString -> Maybe Response
decodeResponse = decode

type KeyHandler = [Event] -> Session -> IO Session
data Session = Session
               { editor :: Editor
               , keyHandler :: KeyHandler }

updateEditor :: (Editor -> Editor) -> Session -> Session
updateEditor f s = s { editor = f (editor s) }

defaultSockAddr = SockAddrInet 6879 iNADDR_ANY

getChan :: IO (Chan Msg)
getChan = do
    chan <- newChan
    forkIO $ fix $ \loop -> do
        -- Read from chan to prevent memory leak
        (_, msg) <- readChan chan
        loop
    return chan

getSocket :: SockAddr -> IO Socket
getSocket addr = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock addr
    listen sock 2
    return sock

runServer :: SockAddr -> IO ()
runServer addr = do
    sock <- getSocket addr
    chan <- getChan
    mainLoop sock chan 0

runServerThread :: SockAddr -> IO ThreadId
runServerThread addr = do
    sock <- getSocket addr
    chan <- getChan
    forkIO $ mainLoop sock chan 0

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr + 1

newSession :: Session
newSession = Session { editor = newEditor
                     , keyHandler = normalMode }

sendResponse :: Socket -> Response -> IO Int64
sendResponse sock = send sock . encodeResponse

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    let broadcast msg = writeChan chan (nr, msg)
    sessionRef <- newIORef newSession
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', str) <- readChan chan'
        when (nr /= nr') $ do
            send sock str
            return ()
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        cmd <- liftM decodeCommand $ recv sock 4096
        case cmd of
            Just cmd -> do
                sess <- readIORef sessionRef >>= handleCommand cmd
                writeIORef sessionRef sess
                sendResponse' sock $ Screen $ editor sess
            Nothing -> sendResponse' sock InvalidCommand
        loop
    killThread reader
    close sock
  where
    sendResponse' sock resp = do
        sendResponse sock resp
        return ()

handleCommand :: Command -> Session -> IO Session
handleCommand (SendKeys evs) s = keyHandler s evs s
handleCommand (EditFile fname) s = do
    r <- liftM R.pack $ readFile fname
    let e = Editor { filePath = Just fname, rope = r, position = R.newPosition }
    return $ s { editor = e }

normalMode :: KeyHandler
normalMode [EvKey (KChar 'h') []] = return . updateEditor cursorLeft
normalMode [EvKey (KChar 'j') []] = return . updateEditor cursorDown
normalMode [EvKey (KChar 'k') []] = return . updateEditor cursorUp
normalMode [EvKey (KChar 'l') []] = return . updateEditor cursorRight
normalMode [EvKey (KChar 'i') []] = \s -> return $ s { keyHandler = insertMode }
normalMode _ = return

insertMode :: KeyHandler
insertMode [EvKey KEsc []] = \s -> return $ s { keyHandler = normalMode }
insertMode [EvKey (KChar c) []] = return . updateEditor (insertChar c)
insertMode [EvKey KEnter []] = return . updateEditor insertNewline
insertMode [EvKey KBS []] = return . updateEditor backspace
insertMode _ = return
