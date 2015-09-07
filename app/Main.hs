module Main where

import Control.Concurrent
import Control.Monad
import Control.Monad.Fix (fix)
import Data.Maybe
import Network.Socket hiding (recv, send, shutdown)
import Network.Socket.ByteString.Lazy (recv, send)
import System.IO
import System.Posix.Signals as Sig

import Graphics.Vty
import qualified Graphics.Vty.Picture as V

import Ebitor.Edit
import Ebitor.Rope (Rope)
import Ebitor.Server
import qualified Ebitor.Rope as R

import Ebitor.Command

renderEditor :: Vty -> Editor -> IO ()
renderEditor vty editor = do
    let ropeLines = R.lines $ rope editor
        img = vertCat $ map (resizeHeight 1 . string defAttr . R.unpack) ropeLines
        R.Cursor (ln, col) = editorCursor editor
        pic = picForImage img
    update vty $ picForImage img
    setCursorPos (outputIface vty) (col - 1) (ln - 1)
    showCursor (outputIface vty)

handleResponse :: Response -> Vty -> IO ()
handleResponse (Screen e) vty = renderEditor vty e
handleResponse InvalidCommand _ = error "InvalidCommand"

eventLoop :: Vty -> Socket -> IO () -> IO ()
eventLoop vty sock loop = do
    e <- nextEvent vty
    case e of
        EvKey (KChar 'q') [MCtrl] -> return ()
        _ -> do { send sock $ encodeCommand $ SendKeys [e] ; loop }

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

main = do
    -- Not working. Why?
    _ <- installHandler sigTSTP Sig.Default Nothing

    vty <- getVty

    runServerThread defaultSockAddr
    sock <- getSocket

    -- Thread to process responses
    forkIO $ fix $ \loop -> do
        resp <- liftM decodeResponse $ recv sock 4096
        when (isJust resp) $ do
            handleResponse (fromJust resp) vty
        loop

    -- send sock $ encodeCommand $ EditFile "app/Main.hs"

    fix $ eventLoop vty sock
    shutdown vty

    close sock
