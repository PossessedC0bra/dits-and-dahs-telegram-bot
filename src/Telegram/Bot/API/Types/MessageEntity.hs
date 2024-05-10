module Telegram.Bot.API.Types.MessageEntity (MessageEntity (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Utilities.Options (reservedKeywordFieldRenameOptions)
import GHC.Generics (Generic)
import Telegram.Bot.API.Types.EntityType (EntityType)

data MessageEntity = MessageEntity
  { type_ :: EntityType,
    offset :: Int,
    length :: Int
  }
  deriving (Show, Generic)

instance FromJSON MessageEntity where
  parseJSON = genericParseJSON reservedKeywordFieldRenameOptions

instance ToJSON MessageEntity where
  toJSON = genericToJSON reservedKeywordFieldRenameOptions