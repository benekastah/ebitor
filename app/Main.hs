module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan (tryPeekTChan)
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Monad.STM (atomically)
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


data App = App
           { term :: Vty
           , serverSocket :: Socket
           , quit :: IO ()
           }


sendCommand :: Socket -> Command -> IO Int64
sendCommand s = send s . encodeCommand

imageForWindow :: Window -> Vty -> Int -> IO Image
imageForWindow win vty height = do
    let ropeLines = R.lines $ contents win
        img = vertCat $ map (resizeHeight 1 . string defAttr . R.unpack) ropeLines
    case cursor win of
        Just (R.Cursor (ln, col)) -> setCursorPos (outputIface vty) (col - 1) (ln - 1)
        Nothing -> return ()
    return $ resizeHeight height img

imageForMessage :: Message -> Image
imageForMessage (Message m) = text' defAttr m
imageForMessage (ErrorMessage m) = text' attr m
  where
    attr = defAttr `withForeColor` white `withBackColor` red

handleResponse :: Response -> App -> IO ()
handleResponse (Screen { editWindow = e, commandBar = c, message = m }) app = do
    (width, height) <- displayBounds $ outputIface vty
    imgE <- imageForWindow e vty (height - 1)
    imgC <- case m of
        Just m' -> if inFocus c then getImgC else return $ imageForMessage m'
        Nothing -> getImgC
    update vty $ picForImage (imgE <-> imgC)
  where
    vty = term app
    getImgC = imageForWindow c vty 1
    inFocus w = isJust $ cursor w
handleResponse Disconnected app = quit app
handleResponse InvalidCommand app = do
    sendCommand (serverSocket app) $ Echo $ ErrorMessage "Invalid command"
    return ()

processEvent :: App -> IO ()
processEvent app = do
    let sock = serverSocket app
    e <- nextEvent $ term app
    sendCommand sock $ SendKeys [e]
    return ()

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

whileRunning :: MVar () -> IO () -> IO ()
whileRunning mvar f = fix $ \loop -> do
    n <- isEmptyMVar mvar
    when n (f >> loop)

main = do
    programStatus <- newEmptyMVar
    vty <- getVty

    -- Not working. Why?
    _ <- installHandler sigTSTP Sig.Default Nothing

    runServerThread defaultSockAddr
    sock <- getSocket

    let app = App
             { term = vty
             , serverSocket = sock
             , quit = putMVar programStatus ()
             }

    -- Thread to process responses
    forkIO $ whileRunning programStatus $ do
        resp <- liftM decodeResponse $ recv sock 4096
        when (isJust resp) $ handleResponse (fromJust resp) app
        showCursor (outputIface vty)
        return ()

    -- Thread to process user events
    forkIO $ whileRunning programStatus $ processEvent app

    -- Wait until we quit the program
    _ <- takeMVar programStatus

    shutdown vty
    close sock
