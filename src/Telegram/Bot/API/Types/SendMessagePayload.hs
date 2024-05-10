{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Bot.API.Types.SendMessagePayload (SendMessagePayload (..)) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data SendMessagePayload = SendMessagePayload
  { chat_id :: Int,
    text :: Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON)