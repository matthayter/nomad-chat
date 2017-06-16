{-# LANGUAGE OverloadedStrings #-}

module RoomsService (
      RoomLookup
    , RoomsService
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
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Data.Time.Format as Time
import           Data.Time (UTCTime, NominalDiffTime)
import           Data.Tuple (swap)
import qualified Data.UUID as UUID
import qualified System.Random as R

data Room = Room {
    getDumpThread :: ThreadId,
    getChan :: Chan T.Text,
    getSubs :: Int,
    lastActive :: UTCTime
}
data User = User {
    name :: T.Text,
    membershipKey :: UUID.UUID
}
type RoomLookup = T.Text -> IO (Maybe (Chan T.Text))
type RoomsService = (MVar [(T.Text, Room)], MVar String)

instance Show Room where
    show room = "Room {" ++ (show $ getSubs room) ++ " subs; activeAt " ++ (Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat (lastActive room)) ++ "}"

default (T.Text)

roomMaxIdle :: NominalDiffTime
roomMaxIdle = fromInteger (180) -- Three minutes

newRoomsService :: IO RoomsService
newRoomsService = do
    rooms <- newMVar ([] :: [(T.Text, Room)])
    randseed <- R.getStdGen
    let infString = randomAlphanumeric randseed
    mvarInfString <- newMVar infString
    let rs = (rooms, mvarInfString)
    -- Fork a thread that closes old rooms after a time
    forkIO $ forever $ do
        threadDelay 10000000 -- Ten seconds
        closeOldRooms rs
    return rs

-- Infinite string of random [a-zA-Z0-9]
randomAlphanumeric :: R.RandomGen g => g -> String
randomAlphanumeric g = (chr . toAlphanumeric) `fmap` R.randomRs (0, 61) g
    where
        toAlphanumeric i | i < 10 = i + 48              -- [0-9]
                         | i < 36 = (i - 10) + 65       -- [A-Z]
                         | otherwise = (i - 36) + 97    -- [a-z]

-- Return a random string of 7 characters by taking the head of the infinite list given, storing the tail back in the MVar
subscribeToRoom :: RoomsService -> T.Text -> IO (Maybe (Chan T.Text))
subscribeToRoom rs@(roomsRef, _) roomName = do
    rooms <- takeMVar $ roomsRef
    currTime <- Time.getCurrentTime
    let (mRoom, newRooms) = lookupReplaceInList roomName (bumpSubs currTime 1) rooms
    putStrLn "New Subscription. Rooms:"
    putStrLn $ show newRooms
    putMVar roomsRef newRooms
    -- Duplicate the channel to return as the subscriber's interface with the room.
    sequence $ freshChan `fmap` mRoom
    where
        freshChan room = Chan.dupChan $ getChan room

unsubscribeFromRoom :: RoomsService -> T.Text -> IO ()
unsubscribeFromRoom rs@(roomsRef, _) roomName = do
    rooms <- takeMVar $ roomsRef
    currTime <- Time.getCurrentTime
    let (_, newRooms) = lookupReplaceInList roomName (bumpSubs currTime (-1)) rooms
    void $ putMVar roomsRef newRooms
    putStrLn "End subscription. Rooms:"
    putStrLn $ show newRooms

withRoom :: RoomsService -> T.Text -> (Chan T.Text -> IO a) -> IO a -> IO a
withRoom rs roomName f noRoom = do
    mChan <- subscribeToRoom rs roomName
    case mChan of
        Just chan ->
            Ex.finally (f chan) (unsubscribeFromRoom rs roomName)
        Nothing -> noRoom

bumpSubs :: UTCTime -> Int -> Room -> Room
bumpSubs currTime n room = room {getSubs = (getSubs room) + n, lastActive = currTime}

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


lookupRoom :: RoomsService -> T.Text -> IO (Maybe (Chan T.Text))
lookupRoom rs roomName = lookupRoom' rs roomName >>= (return . fmap getChan)

lookupRoom' :: RoomsService -> T.Text -> IO (Maybe Room)
lookupRoom' (roomsRef, _) roomName = do
    rooms <- readMVar roomsRef
    return $ lookup roomName rooms

roomExists :: RoomsService -> T.Text -> IO Bool
roomExists rs roomName = (not . isNothing) `fmap` lookupRoom' rs roomName

createRandomRoom :: RoomsService -> IO (T.Text, Chan T.Text)
createRandomRoom rs@(roomsRef, randStrRef) = do
    roomName <- randomRoomName randStrRef
    mRoom <- lookupRoom rs roomName
    case mRoom of
        Nothing -> (,) roomName `fmap` createRoom rs roomName
        Just _ -> createRandomRoom rs

randomRoomName :: MVar String -> IO T.Text
randomRoomName infString = do
    randomShortString <- modifyMVar infString (return . swap . (splitAt 7))
    return $ T.pack randomShortString

createRoom :: RoomsService -> T.Text -> IO (Chan T.Text)
createRoom (roomsRef, _) roomName = do
    rooms <- takeMVar roomsRef
    let mRoomChan2 = lookup roomName rooms
    case mRoomChan2 of
        Just room -> (putMVar roomsRef rooms) >> (return $ getChan room)
        Nothing -> do
            (newRooms, newChan) <- addRoom rooms
            putMVar roomsRef newRooms
            return newChan
    where
        addRoom rooms = do
            newChan <- Chan.newChan
            -- Constantly dump the contents of the new channel
            threadId <- forkIO $ forever $ Chan.readChan newChan
            currentTime <- Time.getCurrentTime
            return ((roomName, (Room threadId newChan 0 currentTime)) : rooms, newChan)

closeOldRooms :: RoomsService -> IO ()
closeOldRooms rs@(roomsRef, _) = do
    rooms <- takeMVar roomsRef
    currTime <- Time.getCurrentTime
    let (freshRooms, oldRooms) = partitionFreshRooms rooms currTime roomMaxIdle
    putMVar roomsRef freshRooms
    void $ sequence $ endRoom `fmap` oldRooms
    where
        endRoom (roomName, room) = do
            putStrLn $ "Closing room: " ++ (show (roomName, room))
            killThread $ getDumpThread room


partitionFreshRooms :: [(T.Text, Room)] -> UTCTime -> Time.NominalDiffTime -> ([(T.Text, Room)], [(T.Text, Room)])
partitionFreshRooms rooms currTime maxIdle =
    partition isFresh rooms
    where
        roomIdle room = Time.diffUTCTime currTime (lastActive room)
        isFresh (roomName, room) = (getSubs room > 0) || (roomIdle room < maxIdle)

getCreateRoom :: RoomsService -> T.Text -> IO (Chan T.Text)
getCreateRoom rs@(roomsRef, _) roomName = do
    mChan <- lookupRoom rs roomName
    case mChan of
        Just chan -> return chan
        -- Recheck for the room: may have been added since our lookup
        Nothing -> createRoom rs roomName

