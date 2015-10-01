{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fprof-auto #-}
module Ebitor.Server
    ( Command(..)
    , Message(..)
    , Response(..)
    , Window(..)
    , defaultSockAddr
    , receiveCommand
    , receiveResponse
    , runServer
    , runServerThread
    , sendCommand
    , sendResponse
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
import Numeric
import System.IO
import System.IO.Error
import qualified Data.Map as M

import Codec.Compression.GZip (compress, decompress)
import Data.Aeson
import System.Log.Handler.Simple
import System.Log.Logger
import qualified Data.ByteString.Lazy as B hiding (pack)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Text.Regex.TDFA as Regex

import Ebitor.Command hiding (commander)
import Ebitor.Edit
import Ebitor.Events
import Ebitor.Language
import Ebitor.Window hiding (focus)
import qualified Ebitor.Command as C
import qualified Ebitor.Rope as R
import qualified Ebitor.Rope.Cursor as R
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
               , displaySize :: (Int, Int)
               }

newSession :: Socket -> Session
newSession sock = Session { editor = newEditor
                          , commandEditor = newEditor
                          , focus = FocusEditor
                          , commander = C.commander
                          , keyHandler = normalMode
                          , clientSocket = sock
                          , lastMessage = Nothing
                          , displaySize = (0, 0)
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

sendData :: Socket -> B.ByteString -> IO Int64
sendData sock dat = do
    debugM loggerName ("sendData length: " ++ show len)
    send sock $ B.append hexLen compressedData
  where
    compressedData = compress dat
    len = B.length compressedData
    hexLen =
        let hex = showHex len ""
            padding = replicate (16 - length hex) '0'
        in  TL.encodeUtf8 (TL.pack $ padding ++ hex)

receiveData :: Socket -> IO B.ByteString
receiveData sock = do
    hexLen <- recv sock 16
    when (B.length hexLen < 16) $ error "Response too short"
    fmap decompress $ recv' (lenFromHex hexLen) ""
  where
    bestLen ((i, ""):_) = i
    bestLen ((i, s):ls) = bestLen ls
    lenFromHex hexLen = bestLen $ readHex $ TL.unpack $ TL.decodeUtf8 hexLen
    recvSize = 4096
    recv' len accum = do
        dat <- recv sock recvSize
        let len' = len - recvSize
            dat' = B.append accum dat
        if len' <= 0 then
            return dat'
        else
            recv' len' dat'

sendResponse :: Socket -> Response -> IO Int64
sendResponse sock = sendData sock . encodeResponse

sendCommand :: Socket -> Command -> IO Int64
sendCommand sock = sendData sock . encodeCommand

receiveResponse :: Socket -> IO (Maybe Response)
receiveResponse = fmap decodeResponse . receiveData

receiveCommand :: Socket -> IO (Maybe Command)
receiveCommand = fmap decodeCommand . receiveData

windowFromEditor :: Editor -> Maybe Int -> Window
windowFromEditor e = window r (snd $ position e)
  where
    r = R.unlines $ drop (firstLine e - 1) (R.lines $ rope e)

getScreen :: Session -> Maybe Message -> Response
getScreen sess msg = Screen win
  where
    displaySize' = displaySize sess
    editWindow = windowFromEditor (editor sess) Nothing
    commandBar = windowFromEditor (commandEditor sess) (Just 1)
    statusBar =
        let windowFromText t = window (R.pack $ T.unpack t) R.newCursor (Just 1)
        in  case msg of
            Just (Message m) -> windowFromText m
            Just (ErrorMessage m) -> windowFromText m
            Nothing -> commandBar
    focusOn = case focus sess of
        FocusEditor -> editWindow
        FocusCommandEditor -> commandBar
    composedWin = editWindow <-> statusBar
    focusedWin = W.focus focusOn composedWin
    win = W.resize focusedWin displaySize'

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
    infoM loggerName ("Client connected: " ++ show nr)
    let broadcast msg = writeChan chan (nr, msg)
    sessionRef <- newIORef $ newSession sock
    chan' <- dupChan chan
    reader <- forkIO $ fix $ \loop -> do
        (nr', str) <- readChan chan'
        when (nr /= nr') $ do
            sendData sock str
            return ()
        loop
    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        infoM loggerName "Waiting for command..."
        cmd <- receiveCommand sock
        case cmd of
            Just cmd -> do
                debugM loggerName ("Command: " ++ show cmd)
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

getIOErrorMessage :: IOError -> T.Text -> T.Text
getIOErrorMessage e thing
    | isDoesNotExistError e = T.concat [thing, " does not exist"]
    | isPermissionError e = T.concat ["You don't have permission to use ", thing]
    | isAlreadyInUseError e = T.concat [thing, " is busy"]
    | otherwise = T.concat [thing, " is unavailable"]

handleCommand :: Command -> Session -> IO Session
handleCommand (CommandSequence cmds) s = foldl (>>=) (return s) $ map handleCommand cmds
handleCommand Disconnect s = do
    let sock = clientSocket s
    sendResponse sock Disconnected
    close sock
    return s
handleCommand (Echo msg) s = return $ s { lastMessage = Just msg }
handleCommand (SendKeys evs) s = keyHandler s evs s
handleCommand (EditFile fname) s = do
    result <- try $ R.readFile fname
    case result of
        Right r -> do
            let e = newEditor { filePath = Just fname, rope = r }
            return $ s { editor = e }
        Left e
            | isDoesNotExistError e ->
                return $ s { editor = (editor s) { filePath = Just fname } }
            | otherwise -> errorMessage $ getIOErrorMessage e $ T.pack fname
  where
    errorMessage msg = return $ s { lastMessage = Just $ ErrorMessage msg }
handleCommand (WriteFile Nothing) s = do
    case filePath $ editor s of
        Just fname -> handleCommand (WriteFile $ Just fname) s
        Nothing -> return $ s { lastMessage = Just $ ErrorMessage "No file name" }
handleCommand (WriteFile (Just fname)) s = do
    let e = editor s
    R.writeFile fname $ rope e
    return $ s { editor = e { filePath = Just fname } }
handleCommand (UpdateDisplaySize size) s = return s { displaySize = size }

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
            infoM loggerName ("Parsed command: " ++ show cmd')
            handleCommand cmd' $ s'
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
