{-# LANGUAGE OverloadedStrings #-}

module RoomsService where

import qualified Data.Text as T
import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar
import qualified Control.Concurrent.Chan as Chan;
    import       Control.Concurrent.Chan (Chan)
import           Control.Monad

type Rooms = [(T.Text, Chan T.Text)]
type RoomProvider = T.Text -> IO (Chan T.Text)
type RoomsService = MVar Rooms

default (T.Text)

newRoomsService :: IO RoomsService
newRoomsService = newMVar ([] :: Rooms)

getCreateRoom :: MVar Rooms -> T.Text -> IO (Chan T.Text)
getCreateRoom roomsRef roomName = do
    mRoomChan <- (lookup roomName) `fmap` readMVar roomsRef
    case mRoomChan of
        Just chan -> return chan
        -- Recheck for the room: may have been added since our lookup
        Nothing -> do
            rooms <- takeMVar roomsRef
            let mRoomChan2 = lookup roomName rooms
            case mRoomChan2 of
                Just chan -> (putMVar roomsRef rooms) >> (return chan)
                Nothing -> do
                    (newRooms, newChan) <- addRoom rooms
                    putMVar roomsRef newRooms
                    return newChan
    where
        addRoom rooms = do
            newChan <- Chan.newChan
            -- Constantly dump the contents of the new channel
            forkIO $ forever $ Chan.readChan newChan
            return ((roomName, newChan) : rooms, newChan)

