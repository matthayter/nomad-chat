{-# LANGUAGE OverloadedStrings #-}

module Main where

-- Unqualified
import Lib
import Messages
import MiddlewareUtil
import OutMessages
import RoomsService
import Types
import UnixProdMode

import Prelude hiding ((++))
import Control.Concurrent.MVar
import Control.Monad
import TextShow

-- Qualified
import qualified Blaze.ByteString.Builder as Blaze
import           Data.Default.Class (def)
import           Control.Concurrent (forkIO, killThread)
import qualified Control.Exception as Ex
import qualified Control.Concurrent.Chan as Chan;
import           Control.Concurrent.Chan (Chan)
import qualified Data.ByteString as BS
import qualified Data.CaseInsensitive as CI
import           Data.Maybe (isNothing)
import           Data.Monoid (mempty, mconcat)
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as T.IO
import qualified Data.Text.Encoding as T.Encoding
import qualified Data.UUID as UUID
import qualified Network.HTTP.Types.Header as Header
import           Network.HTTP.Types.Status (badRequest400, notFound404)
import qualified Network.HTTP.Types.URI as URI
import qualified Network.Wai as WAI;
import           Network.Wai (Application, Middleware)
import           Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import qualified Network.Wai.Handler.WebSockets as WSWai
import qualified Network.WebSockets as WS
import qualified Web.Cookie as Cookie;
import qualified Web.Scotty as Scotty;
import           Web.Scotty (ScottyM, liftAndCatchIO, param, get, post, html, middleware, setHeader, file)
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.FilePath ((</>))
import qualified System.IO as IO

default (Text)

(++) :: Text -> Text -> Text
(++) = T.append

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
            when (not roomExists) $ Scotty.status notFound404 >> Scotty.finish
            setHeader "Content-Type" "text/html; charset=utf-8"
            file $ "public" </> "room.html"
        wsMiddleware $ (chatMiddleware rooms) `requires` findRoomName 
        middleware $ staticPolicy $ hasPrefix "public/"

findRoomName :: WAI.Request -> Maybe Text
findRoomName req =
    case path of
        ("r" : roomName : []) -> Just roomName
        _ -> Nothing
    where
        path = WAI.pathInfo req

findUserName :: WAI.Request -> Maybe Text
findUserName req =
    let 
        textQuery = URI.queryToQueryText $ WAI.queryString req
    in
        join $ lookup "username" textQuery

findUserKey :: WAI.Request -> Maybe UUID.UUID
findUserKey req = do
    cookieHeaderBytes <- lookup Header.hCookie (WAI.requestHeaders req)
    let textCookies = Cookie.parseCookiesText $ cookieHeaderBytes
    userUuidBytes <- lookup userUuidCookieName textCookies
    UUID.fromText userUuidBytes

chatMiddleware :: RoomsService -> RoomName -> Application -> WAI.Request -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
chatMiddleware rs roomName nextApp req res = do
    printT $ mappend "New WS connection to room: " roomName
    let mUserName = findUserName req
        mUserKey = findUserKey req
        -- mChatHandler = (return (chatHandlerValidated rs)) `ap` (return roomName) `ap` mUserName `ap` (return mUserKey) `ap` (return wsHandler) `ap` (return res)
        mChatHandler = (chatHandlerValidated rs) <$> (pure roomName) <*> mUserName <*> pure mUserKey <*> (pure wsHandler) <*> (return res)
    case mChatHandler of
        Just handler -> handler
        Nothing -> res $ WAI.responseLBS badRequest400 [] mempty
    where
        wsHandler = \eRoomSub -> WSWai.websocketsOr WS.defaultConnectionOptions (roomWSServerApp eRoomSub) nextApp req res

-- Try and subscribe - return appropriate errors and error codes if subscription fails
chatHandlerValidated :: RoomsService -> RoomName -> UserName -> Maybe UUID.UUID -> ((Either RoomsError RoomSubscription) -> IO WAI.ResponseReceived) -> (WAI.Response -> IO WAI.ResponseReceived) -> IO WAI.ResponseReceived
chatHandlerValidated rs roomName userName mNameKey wsHandler res = do
    let roomEntry = RoomEntry roomName userName mNameKey
    printT $ "New RoomEntry: " ++ (showt roomEntry)
    withRoom rs roomEntry wsHandler

roomWSServerApp :: (Either RoomsError RoomSubscription) -> WS.PendingConnection -> IO ()
roomWSServerApp eSub p = 
    case eSub of
        Right roomSub -> roomWSServerAppHappy roomSub p
        Left subError -> do
            printT $ "Failed to join room with error: " ++ (showt subError)
            conn <- WS.acceptRequestWith p WS.defaultAcceptRequest
            WS.sendTextData conn $ ErrorMessage subError
            WS.sendClose conn $ ("" :: Text)
            -- WS.rejectRequestWith p (WS.defaultRejectRequest {WS.rejectCode = 402, WS.rejectHeaders = [(toCi "Content-Type", "application/json")], WS.rejectBody = "{\"test\": \"hi\"}"})
            Left ex <- Ex.try $ forever $ ((WS.receiveData conn) :: IO Text)
            case ex of
                WS.CloseRequest _ _ -> putStrLn "WS closed by server"
                _ -> putStrLn "Other exception: " >> (putStrLn $ show (ex :: WS.ConnectionException))


roomWSServerAppHappy :: RoomSubscription -> WS.PendingConnection -> IO ()
roomWSServerAppHappy roomSub p = do
    -- Block on both msgs from the WS connection, and on msgs from the room 'channel'...
    let 
        personalChan = chan roomSub
        rName = roomName roomSub
        userName = name . user $ roomSub
        uuid = secretKey . user $ roomSub
    -- Accept request whilst setting a UUID cookie to match the name
    conn <- WS.acceptRequestWith p (WS.defaultAcceptRequest {WS.acceptHeaders = uuidCookie rName uuid})
    -- Heartbeat. Thread dies silently when connections dies / is closed.
    WS.forkPingThread conn 2
    -- Send successful subscription
    WS.sendTextData conn OutMessages.SubscriptionSuccessfulMessage 
    -- Push new messages to the client
    outgoingWorkerThread <- forkIO $ forever $ do
        outgoing <- Chan.readChan personalChan
        WS.sendTextData conn outgoing

    Left ex <- Ex.try $ forever $ do
        Messages.ChatMessage msg <- WS.receiveData conn
        Chan.writeChan personalChan (OutMessages.ChatMessage msg userName)
    case ex of
        WS.CloseRequest _ _ -> putStrLn "WS closed by remote"
        _ -> putStrLn "Other exception: " >> (putStrLn $ show (ex :: WS.ConnectionException))
    
    killThread outgoingWorkerThread
    putStrLn "Finished WS request"

uuidCookie :: RoomName -> UUID.UUID -> WS.Headers
uuidCookie roomName uuid = [(headerName, headerVal)]
    where
        headerName = toCi "Set-Cookie"
        cookieVal = UUID.toText uuid
        headerVal = Blaze.toByteString $ Cookie.renderSetCookie $ def {
            Cookie.setCookieName = T.Encoding.encodeUtf8 userUuidCookieName,
            Cookie.setCookieValue = T.Encoding.encodeUtf8 cookieVal,
            Cookie.setCookiePath = Just $ T.Encoding.encodeUtf8 $ roomPath roomName
        }

roomPath :: RoomName -> Text
roomPath roomName = "/r/" ++ roomName

userUuidCookieName :: Text
userUuidCookieName = "username-uuid"

toCi :: Text -> CI.CI BS.ByteString
toCi = CI.mk . T.Encoding.encodeUtf8