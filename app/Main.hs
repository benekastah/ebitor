module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan (tryPeekTChan)
import Control.Exception
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Monad.STM (atomically)
import Data.Maybe
import GHC.Int (Int64)
import Network.Socket hiding (shutdown)
import System.Environment (getArgs)
import System.IO
import System.IO.Error
import System.Posix.Signals as Sig

import System.Log.Handler.Simple
import System.Log.Logger

import Data.Aeson (encode)
import Graphics.Vty
import qualified Data.Text as T
import qualified Graphics.Vty.Picture as V

import Ebitor.Edit
import Ebitor.Events.JSON (eventToString)
import Ebitor.Rope (Rope)
import Ebitor.Server
import Ebitor.Utils
import Ebitor.Window (Window(..), Orientation(..))
import qualified Ebitor.Rope as R
import qualified Ebitor.Rope.Cursor as R
import qualified Ebitor.Window as W


data App = App
           { term :: Vty
           , serverSocket :: Socket
           , isRunning :: IO Bool
           , quit :: IO ()
           }

type Window_ = Window (R.Position, R.Rope)


loggerName = "Ebitor.Vty"

setUpLogger :: IO ()
setUpLogger = do
    updateGlobalLogger rootLoggerName removeHandler
    f <- fileHandler "logs/vty.log" DEBUG
    updateGlobalLogger loggerName (setHandlers [f] . setLevel DEBUG)


setCursor :: Vty -> Window_ -> IO ()
setCursor vty w = do
    let out = outputIface vty
        focused = W.getFocusedWindow w
        rect = fromJust $ W.cwRect focused
        R.Cursor (ln, col) = snd . fst $ W.cwContent focused
    setCursorPos out (col - 1 + W.rectX rect) (ln - 1 + W.rectY rect)
    showCursor out

imageForWindow :: Window_ -> Image
imageForWindow w = imageForWindow' w
  where
    imageForWindow' :: Window_ -> Image
    imageForWindow' (LayoutWindow o wins _) =
        let cat = if o == Horizontal then vertCat else horizCat
        in  cat $ map imageForWindow' wins

    imageForWindow' w =
        let ((_, R.Cursor (ln, col)), r) = cwContent w
            img = vertCat $ map imageForLine $ R.lines r
            rect = fromJust $ cwRect w
        in  resizeWidth (W.rectWidth rect) $ resizeHeight (W.rectHeight rect) img

    replaceTabs :: T.Text -> T.Text
    replaceTabs = T.replace "\t" (T.replicate 8 " ")

    imageForLine :: Rope -> Image
    imageForLine = resizeHeight 1 . text' defAttr . replaceTabs . R.unpackText

handleResponse :: Response -> App -> IO ()
handleResponse (Screen w) app = do
    bounds <- displayBounds $ outputIface vty
    update vty $ picForImage $ imageForWindow w
    setCursor vty w
  where
    vty = term app
handleResponse Disconnected app = quit app
handleResponse InvalidCommand app = do
    sendCommand (serverSocket app) $ Echo $ ErrorMessage "Invalid command"
    return ()

processEvent :: App -> IO ()
processEvent app = do
    let sock = serverSocket app
    e <- nextEvent $ term app
    case e of
        EvKey _ _ -> sendCommand sock $ SendKeys [e]
        EvResize w h -> sendCommand sock $ UpdateDisplaySize (w, h)
        _ -> return 0
    return ()

getVty :: IO Vty
getVty = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    setCursorPos (outputIface vty) 0 0
    return vty

getSocket :: IO Socket
getSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    connect sock defaultSockAddr
    infoM loggerName "Connected to server"
    return sock

responseLoop :: App -> IO ()
responseLoop app = whileRunning app $ do
    infoM loggerName "Waiting for response..."
    resp <- receiveResponse (serverSocket app)
    debugM loggerName ("Response: " ++ show resp)
    when (isJust resp) $ handleResponse (fromJust resp) app
    return ()

handleArgs :: App -> IO ()
handleArgs app = do
    args <- getArgs
    case args of
        [] -> return ()
        [fname] -> sendCommand' $ EditFile fname
        _ -> sendCommand' $ Echo (ErrorMessage "Invalid command-line arguments")
  where
    sendCommand' cmd = do
        sendCommand (serverSocket app) cmd
        return ()

whileRunning :: App -> IO () -> IO ()
whileRunning app f = fix $ \loop -> do
    running <- isRunning app
    when running (f >> loop)

main = do
    setUpLogger
    programStatus <- newEmptyMVar
    vty <- getVty

    -- Not working. Why?
    _ <- installHandler sigTSTP Sig.Default Nothing

    sock <- catch getSocket $ \e -> do
        if isDoesNotExistError e then do
            infoM loggerName "Running own server..."
            runServerThread defaultSockAddr
            getSocket
        else
            error "Error connecting to server"

    let app = App
             { term = vty
             , serverSocket = sock
             , isRunning = isEmptyMVar programStatus
             , quit = putMVar programStatus ()
             }

    forkIO $ responseLoop app
    -- Set initial display size
    displayBounds (outputIface vty) >>= sendCommand sock . UpdateDisplaySize

    handleArgs app
    -- Thread to process user events
    forkIO $ whileRunning app $ processEvent app

    -- Wait until we quit the program
    _ <- takeMVar programStatus
    shutdown vty
    close sock
