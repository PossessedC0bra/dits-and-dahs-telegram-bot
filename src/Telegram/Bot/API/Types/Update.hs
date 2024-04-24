{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Bot.API.Types.Update (Update (..)) where

import Data.Aeson
import GHC.Generics (Generic)
import Telegram.Bot.API.Types.Message (Message)

data Update = Update
  { update_id :: Int,
    message :: Maybe Message
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
