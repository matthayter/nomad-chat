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

data ChatMessage = ChatMessage {
    message :: Text
} deriving (Generic, Show)

instance ToJSON ChatMessage where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ChatMessage

newtype MC = MC (Maybe ChatMessage)

instance WS.WebSocketsData ChatMessage where
    fromLazyByteString bs = fromJust $ decode bs
    toLazyByteString = encode

-- data RoomMessage = ChatMessage Text | UserList