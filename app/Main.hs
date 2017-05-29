{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Web.Scotty (scotty, param, get, html, middleware)
import Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WSWai
import Data.Monoid (mconcat)
import Control.Monad (forever)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Control.Exception as Ex
import           Control.Concurrent.Chan (Chan)
import qualified Control.Concurrent.Chan as Chan
import Control.Concurrent (forkIO, killThread)

default (T.Text)

main :: IO ()
main = do
    chan <- Chan.newChan
    scotty 3000 $ do
        middleware $ staticPolicy $ hasPrefix "public/"
        middleware $ WSWai.websocketsOr WS.defaultConnectionOptions $ handleWs chan

-- Block on both msgs from the WS connection, and on msgs from the room 'channel'...
handleWs :: Chan T.Text -> WS.PendingConnection -> IO ()
handleWs chan p = do
    conn <- WS.acceptRequest p
    putStrLn "New connection."
    personalChan <- Chan.dupChan chan
    -- Heartbeat. Thread dies siliently when connections dies / is closed.
    WS.forkPingThread conn 2
    -- Push new messages to the client
    outgoingWorkerThread <- forkIO $ forever $ do
        outgoing <- Chan.readChan personalChan
        WS.sendTextData conn outgoing

    Left ex <- Ex.try $ forever $ do
        msg <- WS.receiveData conn
        T.IO.putStrLn msg
        Chan.writeChan personalChan msg
    case ex of
        WS.CloseRequest _ _ -> putStrLn "WS closed!"
        _ -> putStrLn "Other exception: " >> (putStrLn $ show (ex :: WS.ConnectionException))
    
    killThread outgoingWorkerThread
    putStrLn "Finished WS request"