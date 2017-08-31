{-# LANGUAGE DeriveGeneric #-}

module Messages where

import Data.Aeson
import Data.Maybe
import GHC.Generics

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Network.WebSockets as WS

-- data ChatMessage =
--        ChatMessage Text
--     |  UsersPresentMessage {
--           names :: [Text]
--     }
--     deriving (Generic, Show)

data IncomingNomadMessage = ChatMessage {
    message :: Text
} deriving (Generic, Show)

instance ToJSON IncomingNomadMessage where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON IncomingNomadMessage

instance WS.WebSocketsData IncomingNomadMessage where
    fromDataMessage (WS.Text bs _) = WS.fromLazyByteString bs
    fromDataMessage (WS.Binary bs) = WS.fromLazyByteString bs
    fromLazyByteString bs = fromJust $ decode bs
    toLazyByteString = encode



-- data RoomMessage = ChatMessage Text | UserList