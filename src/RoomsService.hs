{-# LANGUAGE OverloadedStrings #-}

module RoomsService (
      RoomsService
    , RoomSubscription(..)
    , RoomEntry(RoomEntry)
    , RoomName
    , UserName
    , User(..)
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
import           TextShow

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

type RoomOp = ReaderT RoomsService (StateT (Maybe Rooms) (ExceptT RoomsError IO))

data Room = Room {
    getDumpThread :: ThreadId,
    getChan :: Chan T.Text,
    getSubs :: Int,
    lastActive :: UTCTime,
    users :: [User]
}

data User = User {
    name :: UserName,
    secretKey :: UUID.UUID
} deriving Eq

type RoomLookup = T.Text -> IO (Maybe (Chan T.Text))
type RoomName = T.Text
type Rooms = Map.Map RoomName Room
type UserName = T.Text
type RoomChan = Chan T.Text
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

data RoomsError = UserNameTaken
                | IncorrectUserKey
                | RoomDoesNotExist
                deriving Show

instance TextShow RoomsError where
    showb = fromString . show

instance Show Room where
    show room = "Room {" ++ (show $ getSubs room) ++ " subs; activeAt " ++ (Time.formatTime Time.defaultTimeLocale Time.rfc822DateFormat (lastActive room)) ++ "}"

default (T.Text)

runRoomOp :: RoomsService -> (RoomsError -> b) -> (a -> b) -> RoomOp a -> IO b
runRoomOp rs err f roomOp = do
    let unlocked = roomOp >>= (\x -> unlock >> return x)
    let exception = evalStateT (runReaderT unlocked rs) Nothing
    eitherErr <- runExceptT exception
    case eitherErr of
        Left e -> return $ err e
        Right a -> return $ f a

-- 'cleanup op always' sequences 'cleanup' into 'op' regardless of whether 'op' is an error
cleanup :: MonadError e m => m a -> m b -> m a
cleanup maybeFailure always = do
    catchError maybeFailure passthru
    val <- maybeFailure
    always
    return val
    where
        passthru someErr = always >> throwError someErr

roomExists :: RoomsService -> T.Text -> IO Bool
roomExists rs roomName = (not . isNothing) `fmap` lookupRoom' rs roomName

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
    join $ runRoomOp rs (\e -> f (Left e)) (\sub -> safeF sub) roomOpSub
    where
        safeF sub = Ex.finally (f $ Right sub) (unsubscribeFromRoom rs (roomEntryRoomName roomEntry))

createRandomRoom :: RoomsService -> IO (T.Text, Chan T.Text)
createRandomRoom rs@(RoomsService roomsRef randStrRef) = do
    roomName <- randomRoomName randStrRef
    mRoom <- lookupRoom' rs roomName
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
        subscribe' = do
            (room, user) <- addGetUser roomEntry
            sub <- liftIO $ mkNewSubscription room roomName user
            return sub
    in
        cleanup subscribe' unlock

-- Should update rooms in state monad
addGetUser :: RoomEntry -> RoomOp (Room, User)
addGetUser entry = do
    let roomName = roomEntryRoomName entry
    room <- lookupRoom2 roomName
    case (nameKey entry) of
        -- Existing user returning
        Just key ->
            let
                user = User (userName entry) key
            in
                if elem user (users room) then
                    return $ (room, user)
                else
                    throwError IncorrectUserKey
        -- New user, but name might be taken
        Nothing -> do
            newUser <- tryAddUser2 (users room) (userName entry)
            let newRoom = room {users = (newUser : (users room))}
            currTime <- liftIO $ Time.getCurrentTime
            let bumpedRoom = bumpSubs currTime 1 newRoom
            rooms <- getAndLockRooms
            let updatedRooms = Map.insert roomName bumpedRoom rooms
            put (Just updatedRooms)
            return $ (room, newUser)

tryAddUser2 :: [User] -> UserName -> RoomOp User
tryAddUser2 users uName =
    if any (\u -> name u == uName) users then
        throwError UserNameTaken
    else
        liftIO UUID.V4.nextRandom >>= return . (User uName)

getRooms :: RoomOp Rooms
getRooms = do
    (RoomsService roomsMvar _) <- ask
    liftIO $ readMVar roomsMvar

getAndLockRooms :: RoomOp Rooms
getAndLockRooms = do
    mRooms <- get
    case mRooms of
        Just rooms -> return rooms
        Nothing -> do
            rooms <- lock
            put (Just rooms)
            return rooms

lock :: RoomOp Rooms
lock = do
    (RoomsService roomsMvar _) <- ask
    liftIO $ takeMVar roomsMvar

unlock :: RoomOp ()
unlock = do
    mRooms <- get
    (RoomsService roomsMvar _) <- ask
    case mRooms of
        Just rooms -> do
            put Nothing
            liftIO $ putMVar roomsMvar rooms
        Nothing -> return ()

eitherFromMaybe :: l -> Maybe r -> Either l r
eitherFromMaybe l Nothing = Left l
eitherFromMaybe _ (Just r) = Right r

mkNewSubscription :: Room -> RoomName -> User -> IO RoomSubscription
mkNewSubscription room name user = do
    newChan <- Chan.dupChan $ getChan room
    return $ RoomSubscription name newChan user

unsubscribeFromRoom :: RoomsService -> RoomName -> IO ()
unsubscribeFromRoom rs@(RoomsService roomsRef _) roomName = do
    rooms <- takeMVar $ roomsRef
    currTime <- Time.getCurrentTime
    let (_, newRooms) = lookupReplaceInMap roomName (bumpSubs currTime (-1)) rooms
    void $ putMVar roomsRef newRooms
    putStrLn "End subscription. Rooms:"
    putStrLn $ show newRooms

bumpSubs :: UTCTime -> Int -> Room -> Room
bumpSubs currTime n room = room {getSubs = (getSubs room) + n, lastActive = currTime}

-- Reader, IO, Either
lookupRoom2 :: T.Text -> RoomOp Room
lookupRoom2 roomName = do
    rooms <- getAndLockRooms
    let mRoom = Map.lookup roomName rooms
    roomOpFromMaybe mRoom
    where
        roomOpFromMaybe :: Maybe Room -> RoomOp Room
        roomOpFromMaybe (Just room) = return room
        roomOpFromMaybe (Nothing) = throwError RoomDoesNotExist

lookupRoom :: RoomsService -> T.Text -> IO (Maybe (Chan T.Text))
lookupRoom rs roomName = lookupRoom' rs roomName >>= (return . fmap getChan)

lookupRoom' :: RoomsService -> T.Text -> IO (Maybe Room)
lookupRoom' (RoomsService roomsRef _) roomName = do
    rooms <- readMVar roomsRef
    return $ Map.lookup roomName rooms

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