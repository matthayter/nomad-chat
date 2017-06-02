{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import RoomsService
import MiddlewareUtil

import qualified Web.Scotty as Scotty;
    import       Web.Scotty (scotty, param, get, html, middleware, setHeader, file)

import Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import System.FilePath ((</>))
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WSWai
import qualified Network.Wai as WAI;
    import       Network.Wai (Application, Middleware)
import Data.Monoid (mconcat)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Encoding as T.Encoding

import           Control.Concurrent.MVar
import Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as Ex
import qualified Control.Concurrent.Chan as Chan;
    import       Control.Concurrent.Chan (Chan)

default (T.Text)

main :: IO ()
main = do
    rooms <- newRoomsService
    let roomLookup = lookupRoom rooms
    scotty 3000 $ do
        get "/" $ do
            setHeader "Content-Type" "text/html; charset=utf-8"
            file $ "public" </> "index.html"
        httpget "/r/:roomName" $ do
            setHeader "Content-Type" "text/html; charset=utf-8"
            file $ "public" </> "room.html"
        middleware $ staticPolicy $ hasPrefix "public/"
        middleware logHostMiddleware
        wsMiddleware $ require findRoomName (chatMiddleware roomLookup)



findRoomName :: WAI.Request -> Maybe T.Text
findRoomName req = T.stripPrefix "/r/" path
    where
        header = WSWai.getRequestHead req
        path = T.Encoding.decodeUtf8 $ WS.requestPath header

chatMiddleware :: RoomLookup -> T.Text -> Application -> WAI.Request -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
chatMiddleware getRoom roomName nextApp req res =
    let 
        wsHandler = \chan -> WSWai.websocketsOr WS.defaultConnectionOptions (roomWSServerApp chan) nextApp req res
    in do
        T.IO.putStrLn $ T.append "New WS connection to room: " roomName
        mChan <- getRoom roomName
        maybe (nextApp req res) wsHandler mChan

-- Block on both msgs from the WS connection, and on msgs from the room 'channel'...
roomWSServerApp :: Chan T.Text -> WS.PendingConnection -> IO ()
roomWSServerApp chan p = do
    putStrLn "Duplicating room channel."
    conn <- WS.acceptRequest p
    personalChan <- Chan.dupChan chan
    -- Heartbeat. Thread dies silently when connections dies / is closed.
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
