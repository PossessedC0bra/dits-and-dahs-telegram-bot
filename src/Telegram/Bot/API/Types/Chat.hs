module Telegram.Bot.API.Types.Chat (Chat (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Utilities.Options (reservedKeywordFieldRenameOptions)
import Data.Text (Text)
import GHC.Generics (Generic)

data Chat = Chat
  { id :: Int,
    type_ :: Text,
    title :: Maybe Text,
    username :: Maybe Text,
    first_name :: Maybe Text,
    last_name :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Chat where
  parseJSON = genericParseJSON reservedKeywordFieldRenameOptions

instance ToJSON Chat where
  toJSON = genericToJSON reservedKeywordFieldRenameOptions
