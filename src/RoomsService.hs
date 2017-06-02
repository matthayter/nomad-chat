{-# LANGUAGE OverloadedStrings #-}

module RoomsService (
      Rooms
    , RoomLookup
    , newRoomsService
    , lookupRoom
    , createRoom
) where


import qualified Data.Text as T
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
type RoomsService = MVar Rooms

default (T.Text)

newRoomsService :: IO RoomsService
newRoomsService = newMVar ([] :: Rooms)

lookupRoom :: RoomsService -> T.Text -> IO (Maybe (Chan T.Text))
lookupRoom roomsRef roomName = do
    rooms <- readMVar roomsRef
    let mRoom = lookup roomName rooms
    return $ getChan `fmap` mRoom

createRoom :: RoomsService -> T.Text -> IO (Chan T.Text)
createRoom roomsRef roomName = do
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
getCreateRoom roomsRef roomName = do
    mChan <- lookupRoom roomsRef roomName
    case mChan of
        Just chan -> return chan
        -- Recheck for the room: may have been added since our lookup
        Nothing -> createRoom roomsRef roomName

