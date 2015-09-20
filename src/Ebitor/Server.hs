{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Server
    ( Command(..)
    , Message(..)
    , Response(..)
    , Window(..)
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

import System.Log.Handler.Simple
import System.Log.Logger

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Ebitor.Command hiding (commander)
import Ebitor.Edit
import Ebitor.Events
import Ebitor.Language
import Ebitor.Window hiding (focus)
import qualified Ebitor.Command as C
import qualified Ebitor.Rope as R
import qualified Ebitor.Window as W


type Msg = (Int, B.ByteString)


data Response = Screen Window
              | Disconnected
              | InvalidCommand
              deriving (Generic, Show)
instance FromJSON Response
instance ToJSON Response
encodeResponse :: Response -> B.ByteString
encodeResponse = encode
decodeResponse :: B.ByteString -> Maybe Response
decodeResponse = decode

data Focus = FocusEditor | FocusCommandEditor deriving (Show, Eq)

type KeyHandler = [Event] -> Session -> IO Session
data Session = Session
               { editor :: Editor
               , commandEditor :: Editor
               , focus :: Focus
               , commander :: Commander
               , keyHandler :: KeyHandler
               , clientSocket :: Socket
               , lastMessage :: Maybe Message
               }

newSession :: Socket -> Session
newSession sock = Session { editor = newEditor
                          , commandEditor = newEditor
                          , focus = FocusEditor
                          , commander = C.commander
                          , keyHandler = normalMode
                          , clientSocket = sock
                          , lastMessage = Nothing
                          }

updateEditor :: (Editor -> Editor) -> Session -> Session
updateEditor f s = s { editor = f (editor s) }

updateCommandEditor :: (Editor -> Editor) -> Session -> Session
updateCommandEditor f s = s { commandEditor = f (commandEditor s) }

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

loggerName = "Ebitor.Server"

setUpLogger :: IO ()
setUpLogger = do
    updateGlobalLogger rootLoggerName removeHandler
    f <- fileHandler "logs/server.log" DEBUG
    updateGlobalLogger loggerName (setHandlers [f] . setLevel DEBUG)

runServer' :: (IO () -> IO a) -> SockAddr -> IO a
runServer' f addr = do
    setUpLogger
    sock <- getSocket addr
    chan <- getChan
    f $ mainLoop sock chan 0

runServer :: SockAddr -> IO ()
runServer = runServer' id

runServerThread :: SockAddr -> IO ThreadId
runServerThread = runServer' forkIO

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan nr = do
    conn <- accept sock
    forkIO (runConn conn chan nr)
    mainLoop sock chan $! nr + 1

sendResponse :: Socket -> Response -> IO Int64
sendResponse sock resp = do
    infoM loggerName ("Response: " ++ show resp)
    send sock $ encodeResponse resp

windowFromEditor :: Editor -> Int -> Window
windowFromEditor e size = window (rope e) (snd $ position e) size

getScreen :: Session -> Maybe Message -> Response
getScreen sess msg = Screen win'
  where
    editWindow = windowFromEditor (editor sess) 0
    commandBar = windowFromEditor (commandEditor sess) 1
    statusBar =
        let windowFromText t = window (R.pack $ T.unpack t) R.newCursor 1
        in  case msg of
            Just (Message m) -> windowFromText m
            Just (ErrorMessage m) -> windowFromText m
            Nothing -> commandBar
    win = editWindow <-> statusBar
    win' = case focus sess of
        FocusEditor -> W.focus editWindow win
        FocusCommandEditor -> W.focus commandBar win

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    infoM loggerName ("Client connected: " ++ show nr)
    let broadcast msg = writeChan chan (nr, msg)
    sessionRef <- newIORef $ newSession sock
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
                infoM loggerName ("Command: " ++ show cmd)
                sess <- readIORef sessionRef >>= handleCommand cmd
                let msg = lastMessage sess
                writeIORef sessionRef $ sess { lastMessage = Nothing }
                let screen = getScreen sess (lastMessage sess)
                sendResponse' sock screen
            Nothing -> sendResponse' sock InvalidCommand
        loop
    killThread reader
    close sock
  where
    sendResponse' sock resp = do
        sendResponse sock resp
        return ()

handleCommand :: Command -> Session -> IO Session
handleCommand Disconnect s = do
    let sock = clientSocket s
    sendResponse sock Disconnected
    close sock
    return s
handleCommand (Echo msg) s = return $ s { lastMessage = Just msg }
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
normalMode [EvKey (KChar 'i') []] = \s -> return $ toInsertMode s
normalMode [EvKey (KChar ':') []] = \s -> return $ toCommandMode s
normalMode _ = return

baseInsertMode :: ((Editor -> Editor) -> Session -> Session) -> KeyHandler
baseInsertMode _ [EvKey KEsc []] = \s -> return $ toNormalMode s
baseInsertMode update [EvKey (KChar c) []] = return . update (insertChar c)
baseInsertMode update [EvKey KEnter []] = return . update insertNewline
baseInsertMode update [EvKey KBS []] = return . update backspace
baseInsertMode _ _ = return

insertMode :: KeyHandler
insertMode = baseInsertMode updateEditor

cancelCommandMode :: Session -> Session
cancelCommandMode s = (toNormalMode s) { commandEditor = newEditor }

commandMode :: KeyHandler
commandMode [EvKey KEnter []] s =
    case cmd of
        Right cmd' -> do
            handleCommand cmd' s'
        Left e -> return $ s' { lastMessage = Just $ ErrorMessage e }
  where
    s' = cancelCommandMode s
    parseCmd :: R.Rope -> Either T.Text CmdSyntaxNode
    parseCmd r = case parseCommand $ T.pack $ R.unpack r of
        Right result -> Right result
        Left e -> Left $ T.pack $ show e

    cmd :: Either T.Text Command
    cmd = parseCmd (rope $ commandEditor s) >>= getServerCommand (commander s)
commandMode [EvKey KEsc []] s = return $ cancelCommandMode s
commandMode keys s = baseInsertMode updateCommandEditor keys s

toInsertMode :: Session -> Session
toInsertMode s = s { keyHandler = insertMode, focus = FocusEditor }

toNormalMode :: Session -> Session
toNormalMode s = s { keyHandler = normalMode, focus = FocusEditor }

toCommandMode :: Session -> Session
toCommandMode s = s { keyHandler = commandMode, focus = FocusCommandEditor }
