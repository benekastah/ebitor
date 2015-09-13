module Main where

import Control.Concurrent
import Control.Concurrent.STM.TChan (tryPeekTChan)
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Monad.STM (atomically)
import Data.IORef
import Data.Maybe
import GHC.Int (Int64)
import Network.Socket hiding (recv, send, shutdown)
import Network.Socket.ByteString.Lazy (recv, send)
import System.IO
import System.Posix.Signals as Sig

import Data.Aeson (encode)
import Graphics.Vty
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Graphics.Vty.Picture as V

import Ebitor.Edit
import Ebitor.Events.JSON (eventToString)
import Ebitor.Rope (Rope)
import Ebitor.Server
import qualified Ebitor.Rope as R


type ProgramStatus = Bool
programRunning = True
programStopped = False

imageForWindow :: Window -> Vty -> Int -> IO Image
imageForWindow win vty height = do
    let ropeLines = R.lines $ contents win
        img = vertCat $ map (resizeHeight 1 . string defAttr . R.unpack) ropeLines
    case cursor win of
        Just (R.Cursor (ln, col)) -> setCursorPos (outputIface vty) (col - 1) (ln - 1)
        Nothing -> return ()
    return $ resizeHeight height img

handleResponse :: Response -> Vty -> IO ProgramStatus
handleResponse (Screen { editWindow = e, commandBar = c, message = m }) vty =
    keepRunning $ do
        (width, height) <- displayBounds $ outputIface vty
        imgE <- imageForWindow e vty (height - 1)
        imgC <- case m of
            Just m' -> if inFocus c then getImgC else return $ text' defAttr m'
            Nothing -> getImgC
        update vty $ picForImage (imgE <-> imgC)
  where
    getImgC = imageForWindow c vty 1
    inFocus w = isJust $ cursor w
handleResponse Disconnected _ = return programStopped
handleResponse InvalidCommand _ = error "InvalidCommand"

-- Non-blocking nextEvent
tryNextEvent :: Vty -> IO (Maybe Event)
tryNextEvent vty = do
    maybeE <- atomically $ tryPeekTChan $ _eventChannel $ inputIface vty
    if isJust maybeE then do
        fmap Just $ nextEvent vty
    else
        return maybeE

sendCommand :: Socket -> Command -> IO Int64
sendCommand s = send s . encodeCommand

processEvent :: Vty -> Socket -> IO ()
processEvent vty sock = do
    e <- tryNextEvent vty
    case e of
        Just e' -> do
            sendCommand sock $ SendKeys [e']
            return ()
        Nothing -> return ()

getVty :: IO Vty
getVty = do
    cfg <- standardIOConfig
    vty <- mkVty cfg
    setCursorPos (outputIface vty) 0 0
    showCursor (outputIface vty)
    return vty

getSocket :: IO Socket
getSocket = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    connect sock defaultSockAddr
    return sock

programStatusLoop :: IORef ProgramStatus -> IO ProgramStatus -> IO ()
programStatusLoop programStatus f = fix $ \loop -> do
    running <- readIORef programStatus
    if running then do
        running' <- f
        if running /= running' then do
            writeIORef programStatus running'
            return ()
        else
            loop
    else
        return ()

keepRunning :: IO a -> IO ProgramStatus
keepRunning = fmap $ const programRunning

stop :: IO a -> IO ProgramStatus
stop = fmap $ const programStopped

main = do
    programStatus <- newIORef programRunning
    vty <- getVty

    -- Not working. Why?
    _ <- installHandler sigTSTP Sig.Default Nothing

    runServerThread defaultSockAddr
    sock <- getSocket

    -- Thread to process responses
    forkIO $ programStatusLoop programStatus $ do
        resp <- liftM decodeResponse $ recv sock 4096
        running <- case resp of
            Just resp' -> handleResponse resp' vty
            Nothing -> return programRunning
        showCursor (outputIface vty)
        return running

    programStatusLoop programStatus $ keepRunning $ processEvent vty sock

    shutdown vty
    close sock
