module Types where

import Data.Text (Text)
import qualified Data.UUID as UUID

type UserName = Text

data User = User {
    name :: UserName,
    secretKey :: UUID.UUID
} deriving (Eq, Show)
