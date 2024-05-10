module Telegram.Bot.API.Types.EntityType (EntityType (..)) where

import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToJSON)
import Data.Aeson.Utilities.Options (constructorCamelToSnakeCaseOptions)
import GHC.Generics (Generic)

data EntityType = Mention | Hashtag | Cashtag | BotCommand | Url | Email | PhoneNumber | Bold | Italic | Underline | Strikethrough | Spoiler | Blockquote | Code | Pre | TextLink | TextMention | CustomEmoji
  deriving (Show, Generic)

instance ToJSON EntityType where
  toJSON = genericToJSON constructorCamelToSnakeCaseOptions

instance FromJSON EntityType where
  parseJSON = genericParseJSON constructorCamelToSnakeCaseOptions