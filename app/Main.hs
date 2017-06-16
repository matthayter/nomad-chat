{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import RoomsService
import MiddlewareUtil
import UnixProdMode

import           Control.Concurrent.MVar
import           Control.Monad

import           Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as Ex
import qualified Control.Concurrent.Chan as Chan;
import           Control.Concurrent.Chan (Chan)
import           Data.Maybe (isNothing)
import           Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Encoding as T.Encoding
import qualified Network.Wai as WAI;
import           Network.Wai (Application, Middleware)
import           Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import qualified Network.Wai.Handler.WebSockets as WSWai
import qualified Network.WebSockets as WS
import qualified Web.Scotty as Scotty;
import           Web.Scotty (ScottyM, liftAndCatchIO, param, get, post, html, middleware, setHeader, file)
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.FilePath ((</>))
import qualified System.IO as IO

default (T.Text)

main :: IO ()
main = do
    -- Line buffered on output to make the logging consistent.
    IO.hSetBuffering IO.stdout IO.LineBuffering
    -- Prod or dev mode?
    args <- getArgs
    let invalidArgs = die "Required first argument: 'production' or 'development'"
    when (args == []) $ invalidArgs
    let (envMode : _) = args
    scottyMode <- case envMode of
        "development" -> return $ Scotty.scotty 3000
        "production" -> prodModeScotty
        otherwise -> invalidArgs
    web scottyMode

web :: (ScottyM () -> IO ()) -> IO ()
web scotty = do
    rooms <- newRoomsService
    scotty $ do
        httpget "/" $ do
            setHeader "Content-Type" "text/html; charset=utf-8"
            file $ "public" </> "index.html"
        post "/newRoom" $ do
            -- Create room
            (roomName, _) <- liftAndCatchIO $ createRandomRoom rooms
            Scotty.redirect $ TL.fromStrict $ T.append "/r/" roomName
        httpget "/r/:roomName" $ do
            roomName <- param "roomName"
            roomExists <- liftAndCatchIO $ roomExists rooms roomName
            when (not roomExists) Scotty.next
            setHeader "Content-Type" "text/html; charset=utf-8"
            file $ "public" </> "room.html"
        wsMiddleware $ (chatMiddleware rooms) `requires` findRoomName 
        middleware $ staticPolicy $ hasPrefix "public/"

findRoomName :: WAI.Request -> Maybe T.Text
findRoomName req = T.stripPrefix "/r/" path
    where
        header = WSWai.getRequestHead req
        path = T.Encoding.decodeUtf8 $ WS.requestPath header

chatMiddleware :: RoomsService -> T.Text -> Application -> WAI.Request -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
chatMiddleware rs roomName nextApp req res = do
    T.IO.putStrLn $ T.append "New WS connection to room: " roomName
    withRoom rs roomName wsHandler (nextApp req res)
    where
        wsHandler = \chan -> WSWai.websocketsOr WS.defaultConnectionOptions (roomWSServerApp chan) nextApp req res

-- Block on both msgs from the WS connection, and on msgs from the room 'channel'...
roomWSServerApp :: Chan T.Text -> WS.PendingConnection -> IO ()
roomWSServerApp personalChan p = do
    conn <- WS.acceptRequest p
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
