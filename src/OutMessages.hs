{-# LANGUAGE DeriveGeneric #-}

module OutMessages (
    OutgoingNomadMessage (..)
) where

import Types

import Data.Aeson
import Data.Maybe
import GHC.Generics

import qualified Data.Text as T
import           Data.Text (Text)
import qualified Network.WebSockets as WS

data OutgoingNomadMessage = ChatMessage {
    message :: Text,
    chatUser :: UserName
} | UsersMessage {
    names :: [Text]
} | ErrorMessage {
    errorMessage :: RoomsError
} | SubscriptionSuccessfulMessage {
} deriving (Generic, Show)

instance ToJSON OutgoingNomadMessage where
    toEncoding = genericToEncoding defaultOptions

instance ToJSON RoomsError where
   toJSON = toJSON . show
-- instance FromJSON OutgoingNomadMessage

instance WS.WebSocketsData OutgoingNomadMessage where
    -- fromLazyByteString bs = fromJust $ decode bs
    fromLazyByteString = undefined
    fromDataMessage = undefined
    toLazyByteString = encode