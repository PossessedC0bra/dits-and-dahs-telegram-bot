{-# LANGUAGE DeriveAnyClass #-}

module Telegram.Bot.API.Types.User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User
  { id :: Int,
    is_bot :: Bool,
    first_name :: Text,
    last_name :: Maybe Text,
    username :: Maybe Text
  }
  deriving (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
