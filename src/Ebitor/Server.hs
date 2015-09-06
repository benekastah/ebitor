module Ebitor.Server
    ( runServer
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
import Network.Socket hiding (recv, send)
import Network.Socket.ByteString.Lazy (recv, send)
import System.IO

import Data.ByteString.Lazy as B

import Ebitor.Command
import Ebitor.Edit
import qualified Ebitor.Rope as R

type Msg = (Int, B.ByteString)

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

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    let broadcast msg = writeChan chan (nr, msg)
    editorRef <- newIORef $ Editor { filePath = Nothing,
                                     rope = R.empty,
                                     position = R.newPosition }
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', str) <- readChan chan'
        when (nr /= nr') $ do
            send sock str
            return ()
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        cmd <- liftM decodeCommand $ recv sock 4096
        when (isJust cmd) $ do
            editor <- liftM (handleCommand $ fromJust cmd) $ readIORef editorRef
            writeIORef editorRef editor
            send sock $ encodeResponse $ Screen editor
            return ()
        loop
    killThread reader
    close sock

handleCommand :: Command -> Editor -> Editor
handleCommand (InsertChar c) = insertChar c
handleCommand InsertNewline = insertNewline
handleCommand Backspace = backspace
