{-# LANGUAGE OverloadedStrings #-}

module RoomsService (
      Rooms
    , RoomLookup
    , newRoomsService
    , lookupRoom
    , createRandomRoom
) where


import qualified System.Random as R

import qualified Data.Text as T
import           Data.Tuple (swap)
import           Data.Char (chr)
import           Control.Concurrent (ThreadId, forkIO, killThread)
import           Control.Concurrent.MVar
import qualified Control.Concurrent.Chan as Chan;
    import       Control.Concurrent.Chan (Chan)
import           Control.Monad

data Room = Room {
    getDumpThread :: ThreadId,
    getChan :: Chan T.Text
}
type Rooms = [(T.Text, Room)]
type RoomLookup = T.Text -> IO (Maybe (Chan T.Text))
type RoomsService = (MVar Rooms, MVar String)

default (T.Text)

newRoomsService :: IO RoomsService
newRoomsService = do
    rooms <- newMVar ([] :: Rooms)
    randseed <- R.getStdGen
    let infString = randomAlphanumeric randseed
    mvarInfString <- newMVar infString
    return (rooms, mvarInfString)

randomAlphanumeric :: R.RandomGen g => g -> String
randomAlphanumeric g = (chr . toAlphanumeric) `fmap` R.randomRs (0, 61) g
    where
        toAlphanumeric i | i < 10 = i + 48
                         | i < 36 = (i - 10) + 65
                         | otherwise = (i - 36) + 97

randomRoomName :: MVar String -> IO T.Text
randomRoomName infString = do
    randomShortString <- modifyMVar infString (return . swap . (splitAt 7))
    return $ T.pack randomShortString

lookupRoom :: RoomsService -> T.Text -> IO (Maybe (Chan T.Text))
lookupRoom (roomsRef, _) roomName = do
    rooms <- readMVar roomsRef
    let mRoom = lookup roomName rooms
    return $ getChan `fmap` mRoom

createRandomRoom :: RoomsService -> IO (T.Text, Chan T.Text)
createRandomRoom rs@(roomsRef, randStrRef) = do
    roomName <- randomRoomName randStrRef
    mRoom <- lookupRoom rs roomName
    case mRoom of
        Nothing -> (,) roomName `fmap` createRoom rs roomName
        Just _ -> createRandomRoom rs

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
            return ((roomName, (Room threadId newChan)) : rooms, newChan)

getCreateRoom :: RoomsService -> T.Text -> IO (Chan T.Text)
getCreateRoom rs@(roomsRef, _) roomName = do
    mChan <- lookupRoom rs roomName
    case mChan of
        Just chan -> return chan
        -- Recheck for the room: may have been added since our lookup
        Nothing -> createRoom rs roomName

