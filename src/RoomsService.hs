{-# LANGUAGE OverloadedStrings #-}

module RoomsService (
      RoomsService
    , RoomSubscription(..)
    , RoomEntry(RoomEntry)
    , RoomName
    , RoomsError
    , newRoomsService
    , roomExists
    , withRoom
    , createRandomRoom
) where

-- Unqualified
import           Data.List
import           Data.Maybe
import           Control.Concurrent.MVar
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Messages
import           OutMessages
import           TextShow
import           Types

import           Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan;
    import       Control.Concurrent.Chan (Chan)
import qualified Control.Exception as Ex
import           Control.Monad
import           Data.Char (chr)
import qualified Data.Map as Map
import qualified Data.Text as T
import           Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.Time.Format as Time
import           Data.Time (UTCTime, NominalDiffTime)
import           Data.Tuple (swap)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified System.Random as R

type RoomOp = ReaderT RoomsService (ExceptT RoomsError IO)

data Room = Room {
    getDumpThread :: ThreadId,
    getChan :: RoomChan,
    getSubs :: [UserName],
    lastActive :: UTCTime,
    users :: [User]
}

type RoomChan = Chan OutgoingNomadMessage
type RoomName = Text
type Rooms = Map.Map RoomName Room
data RoomsService = RoomsService (MVar Rooms) (MVar String)

data RoomEntry = RoomEntry {
    roomEntryRoomName   :: RoomName,
    userName            :: UserName,
    nameKey             :: Maybe UUID.UUID -- The UUID associated with an existing name, if any.
} deriving Show

instance TextShow RoomEntry where
    showb = fromString . show

data RoomSubscription = RoomSubscription {
    roomName :: RoomName,
    chan :: RoomChan,
    user :: User
}

instance TextShow RoomsError where
    showb = fromString . show

instance Show Room where
    show room = "Room {" ++ (show $ getSubs room) ++ " subs; activeAt " ++ (Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat (lastActive room)) ++ "}"

default (Text)

runRoomOp :: RoomsService -> (RoomsError -> b) -> (a -> b) -> RoomOp a -> IO b
runRoomOp rs err f roomOp = do
    let exception = runReaderT roomOp rs
    eitherErr <- runExceptT exception
    case eitherErr of
        Left e -> return $ err e
        Right a -> return $ f a

execRoomOp :: RoomsService -> RoomOp a -> IO ()
execRoomOp rs roomOp = join $ runRoomOp rs (\err -> putStrLn ("RoomOp failed with err: " ++ show err)) (const $ return ()) roomOp

roomExists :: RoomsService -> Text -> IO Bool
roomExists rs roomName = (not . isNothing) `fmap` lookupRoom rs roomName

newRoomsService :: IO RoomsService
newRoomsService = do
    rooms <- newMVar (Map.empty)
    randseed <- R.getStdGen
    let infString = randomAlphanumeric randseed
    mvarInfString <- newMVar infString
    let rs = RoomsService rooms mvarInfString
    -- Fork a thread that closes old rooms after a time
    forkIO $ forever $ do
        threadDelay 10000000 -- Ten seconds
        closeOldRooms rs
    return rs

withRoom :: RoomsService -> RoomEntry -> (Either RoomsError RoomSubscription -> IO a) -> IO a
withRoom rs roomEntry f = do
    let roomOpSub = subscribeToRoom roomEntry
    join $ runRoomOp rs (\err -> f (Left err)) (\sub -> safeF sub) roomOpSub
    where
        safeF sub = Ex.finally (f $ Right sub) (unsub sub)
        unsub sub = execRoomOp rs (unsubscribeFromRoom sub roomName)
        roomName = roomEntryRoomName roomEntry

createRandomRoom :: RoomsService -> IO (Text, RoomChan)
createRandomRoom rs@(RoomsService roomsRef randStrRef) = do
    roomName <- randomRoomName randStrRef
    mRoom <- lookupRoom rs roomName
    case mRoom of
        Nothing -> do
            newRoom <- createRoom rs roomName
            return $ (roomName, getChan newRoom)
        Just _ -> createRandomRoom rs

roomMaxIdle :: NominalDiffTime
roomMaxIdle = fromInteger (180) -- Three minutes

subscribeToRoom :: RoomEntry -> RoomOp (RoomSubscription)
subscribeToRoom roomEntry =
    let
        roomName = roomEntryRoomName roomEntry
        subscribe' rooms = do
            room <- roomOpFromMaybe RoomDoesNotExist $ Map.lookup roomName rooms
            (updatedRoom, sub) <- addGetUser room roomEntry
            let updatedRooms = Map.insert roomName updatedRoom rooms
            -- sub <- liftIO $ mkNewSubscription updatedRoom roomName user
            unlock updatedRooms
            liftIO $ broadcastMembers updatedRoom
            return sub
    in do
        rooms <- lock
        (subscribe' rooms) `catchError` (\err -> unlock rooms >> throwError err)

unsubscribeFromRoom :: RoomSubscription -> RoomName -> RoomOp ()
unsubscribeFromRoom sub roomName =
    let
        unsubscribeFromRoom' rooms = do
            room <- roomOpFromMaybe RoomDoesNotExist $ Map.lookup roomName rooms
            updatedRoom <- rmSub sub room
            let newRooms = Map.insert roomName updatedRoom rooms
            unlock newRooms
            liftIO $ broadcastMembers updatedRoom
            liftIO $ putStrLn "End subscription. Rooms:"
            liftIO $ putStrLn $ show newRooms
    in do
        rooms <- lock
        (unsubscribeFromRoom' rooms) `catchError` (\err -> unlock rooms >> throwError err)

-- addGetUser :: Room -> RoomEntry -> RoomOp (Rooms, Room, User)

broadcastMembers :: Room -> IO ()
broadcastMembers room =
    Chan.writeChan chan usersListMsg
    where
        chan = getChan room
        usersListMsg = UsersMessage userNames
        userNames = getSubs room

addGetUser :: Room -> RoomEntry -> RoomOp (Room, RoomSubscription)
addGetUser room entry = do
    let roomName = roomEntryRoomName entry
    case (nameKey entry) of
        -- Existing user returning
        Just key -> do
            let user = User (userName entry) key
            verifyUser user (users room)
            sub <- liftIO $ mkNewSubscription (getChan room) roomName user
            bumpedRoom <- addSub sub room
            return $ (bumpedRoom, sub)

        -- New user, but name might be taken
        Nothing -> do
            newUser <- tryAddUser (users room) (userName entry)
            let roomPlusUser = room {users = (newUser : (users room))}
            sub <- liftIO $ mkNewSubscription (getChan room) roomName newUser
            roomPlusSub <- addSub sub roomPlusUser
            return $ (roomPlusSub, sub)

verifyUser :: User -> [User] -> RoomOp ()
verifyUser user users =
    if elem user users then
        return ()
    else
        throwError IncorrectUserKey

tryAddUser :: [User] -> UserName -> RoomOp User
tryAddUser users uName =
    if any (\u -> Types.name u == uName) users then
        throwError UserNameTaken
    else
        liftIO UUID.V4.nextRandom >>= return . (User uName)

getRooms :: RoomOp Rooms
getRooms = do
    (RoomsService roomsMvar _) <- ask
    liftIO $ readMVar roomsMvar

lock :: RoomOp Rooms
lock = do
    (RoomsService roomsMvar _) <- ask
    liftIO $ takeMVar roomsMvar

unlock :: Rooms -> RoomOp ()
unlock rooms = do
    (RoomsService roomsMvar _) <- ask
    liftIO $ putMVar roomsMvar rooms

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l Nothing = Left l
eitherFromMaybe _ (Just r) = Right r

mkNewSubscription :: RoomChan -> RoomName -> User -> IO RoomSubscription
mkNewSubscription chan name user = do
    newChan <- Chan.dupChan chan
    return $ RoomSubscription name newChan user

rmSub :: RoomSubscription -> Room -> RoomOp (Room)
rmSub sub room = do
    currTime <- liftIO Time.getCurrentTime
    let initSubs = getSubs room
    let uName = Types.name (RoomsService.user sub)
    let updatedSubs = delete uName initSubs
    return $ room {getSubs = updatedSubs, lastActive = currTime}

addSub :: RoomSubscription -> Room -> RoomOp (Room)
addSub sub room = do
    currTime <- liftIO Time.getCurrentTime
    let initSubs = getSubs room
    let uName = Types.name (user sub)
    return $ room {getSubs = (uName : initSubs), lastActive = currTime}

roomOpFromMaybe :: RoomsError -> Maybe a -> RoomOp a
roomOpFromMaybe _ (Just a) = return a
roomOpFromMaybe err (Nothing) = throwError err

lookupRoom :: RoomsService -> Text -> IO (Maybe Room)
lookupRoom (RoomsService roomsRef _) roomName = do
    rooms <- readMVar roomsRef
    return $ Map.lookup roomName rooms

-- Return a random string of 7 characters by taking the head of the infinite list given, storing the tail back in the MVar
randomRoomName :: MVar String -> IO Text
randomRoomName infString = do
    randomShortString <- modifyMVar infString (return . swap . (splitAt 7))
    return $ T.pack randomShortString

createRoom :: RoomsService -> Text -> IO Room
createRoom rs@(RoomsService roomsRef _) roomName = do
    rooms <- takeMVar roomsRef
    let mRoom = Map.lookup roomName rooms
    case mRoom of
        Just room -> (putMVar roomsRef rooms) >> (return room)
        Nothing -> do
            newRoom <- newRoomIO
            putMVar roomsRef (Map.insert roomName newRoom rooms)
            return newRoom
    where
        newRoomIO = do
            newChan <- Chan.newChan
            -- Constantly dump the contents of the new channel
            threadId <- forkIO $ forever $ Chan.readChan newChan
            currentTime <- Time.getCurrentTime
            return $ Room threadId newChan [] currentTime []

closeOldRooms :: RoomsService -> IO ()
closeOldRooms rs@(RoomsService roomsRef _) = do
    rooms <- takeMVar roomsRef
    currTime <- Time.getCurrentTime
    let roomIdle room = Time.diffUTCTime currTime (lastActive room)
    let isFresh room = ((getSubs room) /= []) || (roomIdle room < roomMaxIdle)
    let (freshRooms, oldRooms) = Map.partition isFresh rooms
    putMVar roomsRef freshRooms
    void $ sequence $ endRoom `Map.mapWithKey` oldRooms
    where
        endRoom roomName room = do
            putStrLn $ "Closing room: " ++ (show (roomName, room))
            killThread $ getDumpThread room

-- Helpers
lookupReplaceInMap :: Ord k => k -> (v -> v) -> Map.Map k v -> (Maybe v, Map.Map k v)
lookupReplaceInMap k f m = Map.updateLookupWithKey (\x y -> return $ f y) k m

lookupReplaceInList :: Eq a => a -> (b -> b) -> [(a,b)] -> (Maybe b, [(a,b)])
lookupReplaceInList _ _ [] = (Nothing, [])
lookupReplaceInList key mutator (x:xs) =
    if (key == fst x) then
        let
            newVal = mutator $ snd x
            newList = (key, newVal) : xs
        in
            (Just (snd x), newList)
    else
        let (res, l) = lookupReplaceInList key mutator xs in (res, x : l)

-- Infinite string of random [a-zA-Z0-9]
randomAlphanumeric :: R.RandomGen g => g -> String
randomAlphanumeric g = (chr . toAlphanumeric) `fmap` R.randomRs (0, 61) g
    where
        toAlphanumeric i | i < 10 = i + 48              -- [0-9]
                         | i < 36 = (i - 10) + 65       -- [A-Z]
                         | otherwise = (i - 36) + 97    -- [a-z]