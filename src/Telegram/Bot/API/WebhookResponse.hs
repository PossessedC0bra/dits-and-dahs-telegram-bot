module Telegram.Bot.API.WebhookResponse (WebhookResponse (..), Method (..)) where

import Data.Aeson (Options (tagSingleConstructors), ToJSON (toJSON), Value (Object), genericToJSON, object, (.=))
import Data.Aeson.Utilities.Options (constructorLowercaseFirstOptions)
import GHC.Generics (Generic)

data WebhookResponse a = WebhookResponse
  { method :: Method,
    value :: a
  }
  deriving (Generic)

instance (ToJSON a) => ToJSON (WebhookResponse a) where
  toJSON (WebhookResponse method value)
    | Object obj <- jsonValue = Object $ obj <> ("method" .= method)
    | otherwise = object ["method" .= method, "value" .= jsonValue]
    where
      jsonValue = toJSON value

data Method = SendMessage
  deriving (Generic)

instance ToJSON Method where
  toJSON = genericToJSON constructorLowercaseFirstOptions {tagSingleConstructors = True}