{-# LANGUAGE OverloadedStrings #-}

module RoomsService (
      RoomLookup
    , RoomsService
    , RoomSubscription(..)
    , RoomEntry(RoomEntry)
    , RoomName
    , UserName
    , newRoomsService
    , roomExists
    , withRoom
    , createRandomRoom
) where

-- Unqualified
import           Data.List
import           Data.Maybe
import           Control.Concurrent.MVar

import           Control.Concurrent (ThreadId, threadDelay, forkIO, killThread)
import qualified Control.Concurrent.Chan as Chan;
    import       Control.Concurrent.Chan (Chan)
import qualified Control.Exception as Ex
import           Control.Monad
import           Data.Char (chr)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Format as Time
import           Data.Time (UTCTime, NominalDiffTime)
import           Data.Tuple (swap)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified System.Random as R

data Room = Room {
    getDumpThread :: ThreadId,
    getChan :: Chan T.Text,
    getSubs :: Int,
    lastActive :: UTCTime,
    users :: [User]
}

data User = User {
    name :: UserName,
    membershipKey :: UUID.UUID
} deriving Eq

type RoomLookup = T.Text -> IO (Maybe (Chan T.Text))
type RoomName = T.Text
type UserName = T.Text
type RoomChan = Chan T.Text
data RoomsService = RoomsService (MVar (Map.Map RoomName Room)) (MVar String)

data RoomEntry = RoomEntry {
    roomEntryRoomName   :: RoomName,
    userName            :: UserName,
    nameKey             :: Maybe UUID.UUID -- The UUID associated with an existing name, if any.
}

data RoomSubscription = RoomSubscription {
    roomName :: RoomName,
    chan :: RoomChan,
    user :: User
}

data RoomsError = UserNameTaken
                | IncorrectUserKey
                | RoomDoesNotExist

instance Show Room where
    show room = "Room {" ++ (show $ getSubs room) ++ " subs; activeAt " ++ (Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat (lastActive room)) ++ "}"

default (T.Text)

roomMaxIdle :: NominalDiffTime
roomMaxIdle = fromInteger (180) -- Three minutes

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

subscribeToRoom :: RoomsService -> RoomEntry -> IO (Either RoomsError RoomSubscription)
subscribeToRoom rs@(RoomsService roomsRef _) roomEntry = do
    rooms <- takeMVar $ roomsRef
    currTime <- Time.getCurrentTime
    let eSelectedRoom = eitherFromMaybe RoomDoesNotExist $ Map.lookup roomName rooms
    eAddResult <- sequence $ eSelectedRoom >>= (addGetUser roomEntry)
    case eAddResult of
        Right (newRoom, user) -> do
            putMVar roomsRef (Map.insert roomName newRoom rooms)
            sub <- mkNewSubscription newRoom roomName user
            return $ Right sub
        Left e -> putMVar roomsRef rooms >> return (Left e)
    -- Left: putMVar oldrooms, return Left
    -- Right: putMVar new rooms, return Right with new room sub

    where
        roomName = roomEntryRoomName roomEntry

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l Nothing = Left l
eitherFromMaybe _ (Just r) = Right r

addGetUser :: RoomEntry -> Room -> Either RoomsError (IO (Room, User))
addGetUser entry room =
    case (nameKey entry) of
        -- Existing user returning
        Just key ->
            let
                user = User (userName entry) key
            in
                if elem user (users room) then
                    Right $ return $ (room, user)
                else
                    Left IncorrectUserKey
        -- New user, but name might be taken
        Nothing -> do
            ioNewUser <- tryAddUser (users room) (userName entry)
            return $ do
                newUser <- ioNewUser
                return $ (room {users = (newUser : (users room))}, newUser)


tryAddUser :: [User] -> UserName -> Either RoomsError (IO User)
tryAddUser users uName =
    if any (\u -> name u == uName) users then
        Left UserNameTaken
    else
        Right $ do
            newUuid <- UUID.V4.nextRandom
            return $ User uName newUuid

mkNewSubscription :: Room -> RoomName -> User -> IO RoomSubscription
mkNewSubscription room name user = do
    newChan <- Chan.dupChan $ getChan room
    return $ RoomSubscription name newChan user

unsubscribeFromRoom :: RoomsService -> T.Text -> IO ()
unsubscribeFromRoom rs@(RoomsService roomsRef _) roomName = do
    rooms <- takeMVar $ roomsRef
    currTime <- Time.getCurrentTime
    let (_, newRooms) = lookupReplaceInMap roomName (bumpSubs currTime (-1)) rooms
    void $ putMVar roomsRef newRooms
    putStrLn "End subscription. Rooms:"
    putStrLn $ show newRooms

withRoom :: RoomsService -> RoomEntry -> (Either RoomsError RoomSubscription -> IO a) -> IO a
withRoom rs roomEntry f = do
    eSub <- subscribeToRoom rs roomEntry
    Ex.finally (f $ eSub) (unsubscribeFromRoom rs (roomEntryRoomName roomEntry))

bumpSubs :: UTCTime -> Int -> Room -> Room
bumpSubs currTime n room = room {getSubs = (getSubs room) + n, lastActive = currTime}

lookupRoom :: RoomsService -> T.Text -> IO (Maybe (Chan T.Text))
lookupRoom rs roomName = lookupRoom' rs roomName >>= (return . fmap getChan)

lookupRoom' :: RoomsService -> T.Text -> IO (Maybe Room)
lookupRoom' (RoomsService roomsRef _) roomName = do
    rooms <- readMVar roomsRef
    return $ Map.lookup roomName rooms

roomExists :: RoomsService -> T.Text -> IO Bool
roomExists rs roomName = (not . isNothing) `fmap` lookupRoom' rs roomName

createRandomRoom :: RoomsService -> IO (T.Text, Chan T.Text)
createRandomRoom rs@(RoomsService roomsRef randStrRef) = do
    roomName <- randomRoomName randStrRef
    mRoom <- lookupRoom' rs roomName
    case mRoom of
        Nothing -> do
            newRoom <- createRoom rs roomName
            return $ (roomName, getChan newRoom)
        Just _ -> createRandomRoom rs

-- Return a random string of 7 characters by taking the head of the infinite list given, storing the tail back in the MVar
randomRoomName :: MVar String -> IO T.Text
randomRoomName infString = do
    randomShortString <- modifyMVar infString (return . swap . (splitAt 7))
    return $ T.pack randomShortString

createRoom :: RoomsService -> T.Text -> IO Room
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
            return $ Room threadId newChan 0 currentTime []

closeOldRooms :: RoomsService -> IO ()
closeOldRooms rs@(RoomsService roomsRef _) = do
    rooms <- takeMVar roomsRef
    currTime <- Time.getCurrentTime
    let roomIdle room = Time.diffUTCTime currTime (lastActive room)
    let isFresh room = (getSubs room > 0) || (roomIdle room < roomMaxIdle)
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