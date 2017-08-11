{-# LANGUAGE DeriveGeneric #-}

module OutMessages where

import Data.Aeson
import Data.Maybe
import GHC.Generics

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Network.WebSockets as WS

data OutgoingNomadMessage = ChatMessage {
    message :: Text
} | UsersMessage {
    name :: Text
} deriving (Generic, Show)

instance ToJSON OutgoingNomadMessage where
    toEncoding = genericToEncoding defaultOptions

-- instance FromJSON OutgoingNomadMessage

instance WS.WebSocketsData OutgoingNomadMessage where
    -- fromLazyByteString bs = fromJust $ decode bs
    fromLazyByteString = undefined
    toLazyByteString = encode