module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan (tryPeekTChan)
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Monad.STM (atomically)
import Data.Maybe
import GHC.Int (Int64)
import Network.Socket hiding (shutdown)
import System.Environment (getArgs)
import System.IO
import System.Posix.Signals as Sig

import System.Log.Handler.Simple
import System.Log.Logger

import Data.Aeson (encode)
import Graphics.Vty
import qualified Data.ByteString.Lazy.Char8 as B
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


loggerName = "Ebitor.Vty"

setUpLogger :: IO ()
setUpLogger = do
    updateGlobalLogger rootLoggerName removeHandler
    f <- fileHandler "logs/vty.log" DEBUG
    updateGlobalLogger loggerName (setHandlers [f] . setLevel DEBUG)


setCursor :: Vty -> Window -> IO ()
setCursor vty w = do
    let out = outputIface vty
    (fullWidth, fullHeight) <- displayBounds out
    case getCursor Horizontal (-1, -1, fullWidth, fullHeight) w of
        Just (ln, col) -> do
            setCursorPos out col ln
            showCursor out
        Nothing -> hideCursor out
  where
    getCursor :: Orientation -> (Int, Int, Int, Int) -> Window -> Maybe (Int, Int)
    getCursor _ offset (LayoutWindow o wins) =
        let advanceOffset (offsetLn, offsetCol, width, height) w = if o == Horizontal then
                (offsetLn + W.height height w, offsetCol, width, height)
            else
                (offsetLn, offsetCol + W.width width w, width, height)
            getCursor' _ [] = Nothing
            getCursor' offset (w:wins) = case getCursor o offset w of
                Nothing -> getCursor' (advanceOffset offset w) wins
                curs -> curs
        in  getCursor' offset wins
    getCursor _ (offsetLn, offsetCol, _, _) (ContentWindow _ (R.Cursor (ln, col)) _ f) =
        let vtyLn = offsetLn + ln
            vtyCol = offsetCol + col
        in  if f then Just (vtyLn, vtyCol) else Nothing

imageForWindow :: Window -> Image
imageForWindow w = imageForWindow' Horizontal w
  where
    imageForWindow' :: Orientation -> Window -> Image
    imageForWindow' _ (LayoutWindow o wins) =
        let cat = if o == Horizontal then vertCat else horizCat
        in  cat $ map (imageForWindow' o) wins

    imageForWindow' o (ContentWindow r curs@(R.Cursor (ln, col)) s f) =
        let img = vertCat $ map imageForLine $ R.lines r
            resizeDimension = if o == Horizontal then resizeHeight else resizeWidth
        in  resizeDimension (maybe 0 id s) img

    replaceTabs :: String -> String
    replaceTabs = concatMap (\c -> if c == '\t' then spaces else [c])
      where
        spaces = replicate 8 ' '

    imageForLine :: Rope -> Image
    imageForLine = resizeHeight 1 . string defAttr . replaceTabs . R.unpack

handleResponse :: Response -> App -> IO ()
handleResponse (Screen w) app = do
    bounds <- displayBounds $ outputIface vty
    let w' = W.resize w bounds
    update vty $ picForImage $ imageForWindow w'
    setCursor vty w'
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

    runServerThread defaultSockAddr
    sock <- getSocket

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
