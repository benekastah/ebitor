{-# LANGUAGE DeriveGeneric #-}
module Ebitor.Server
    ( Command(..)
    , Message
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

import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T

import Ebitor.Command hiding (commander)
import Ebitor.Edit
import Ebitor.Events
import Ebitor.Language
import qualified Ebitor.Rope as R
import qualified Ebitor.Command as C


type Msg = (Int, B.ByteString)


data Window = Window { contents :: R.Rope, cursor :: Maybe R.Cursor }
            deriving (Generic, Show)
instance FromJSON Window
instance ToJSON Window

type Message = T.Text

data Response = Screen
                { editWindow :: Window
                , commandBar :: Window
                , message :: Maybe Message
                }
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
               }

newSession :: Socket -> Session
newSession sock = Session { editor = newEditor
                          , commandEditor = newEditor
                          , focus = FocusEditor
                          , commander = C.commander
                          , keyHandler = normalMode
                          , clientSocket = sock
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

sendResponse :: Socket -> Response -> IO Int64
sendResponse sock = send sock . encodeResponse

windowFromEditor :: Editor -> Bool -> Window
windowFromEditor e inFocus = Window { contents = (rope e)
                                    , cursor = c
                                    }
  where
    c = if inFocus then Just (snd $ position e) else Nothing

getScreen :: Session -> Maybe Message -> Response
getScreen sess msg =
    Screen
    { editWindow = windowFromEditor (editor sess) (focus sess == FocusEditor)
    , commandBar = windowFromEditor (commandEditor sess) (focus sess == FocusCommandEditor)
    , message = msg
    }

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan nr = do
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
                sess <- readIORef sessionRef >>= handleCommand cmd
                writeIORef sessionRef sess
                let screen = getScreen sess Nothing
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
handleCommand (Echo msg) s =
    sendResponse (clientSocket s) (getScreen s $ Just msg) >> return s
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

commandMode :: KeyHandler
commandMode [EvKey KEnter []] s =
    case cmd of
        Right cmd' -> do
            handleCommand cmd' (toNormalMode s)
        -- TODO proper error handling
        Left e -> error $ T.unpack e
  where
    parseCmd :: R.Rope -> Either T.Text CmdSyntaxNode
    parseCmd r = case parseCommand $ T.pack $ R.unpack r of
        Right result -> Right result
        Left e -> Left $ T.pack $ show e

    cmd :: Either T.Text Command
    cmd = parseCmd (rope $ commandEditor s) >>= getServerCommand (commander s)
commandMode [EvKey KEsc []] s = return $ (toNormalMode s) { commandEditor = newEditor }
commandMode keys s = baseInsertMode updateCommandEditor keys s

toInsertMode :: Session -> Session
toInsertMode s = s { keyHandler = insertMode, focus = FocusCommandEditor }

toCommandMode :: Session -> Session
toCommandMode s = s { keyHandler = commandMode, focus = FocusCommandEditor }

toNormalMode :: Session -> Session
toNormalMode s = s { keyHandler = normalMode, focus = FocusEditor }
