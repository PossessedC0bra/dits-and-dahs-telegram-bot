{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Bot.API.Types.Message (Message (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Telegram.Bot.API.Types.Chat (Chat)
import Telegram.Bot.API.Types.MessageEntity (MessageEntity)
import Telegram.Bot.API.Types.User (User)

data Message = Message
  { message_id :: Int,
    from :: User,
    chat :: Chat,
    date :: Int,
    text :: Maybe Text,
    entities :: Maybe [MessageEntity]
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)