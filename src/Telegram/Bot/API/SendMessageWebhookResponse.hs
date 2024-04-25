{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Bot.API.SendMessageWebhookResponse (SendMessageWebhookResponse, sendMessageWebhookResponse) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data SendMessageWebhookResponse = WebhookResponse
  { method :: Text,
    chat_id :: Int,
    text :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

sendMessageWebhookResponse :: Int -> Text -> SendMessageWebhookResponse
sendMessageWebhookResponse = WebhookResponse "sendMessage"