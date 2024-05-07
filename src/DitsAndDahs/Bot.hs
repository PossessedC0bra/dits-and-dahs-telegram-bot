module DitsAndDahs.Bot (convertText) where

import Data.Text (Text, all, elem)
import DitsAndDahs.MorseCode (fromMorseCode, toMorseCode)

convertText :: Text -> Text
convertText text
  | isMorseCode text = fromMorseCode text
  | otherwise = toMorseCode text

isMorseCode :: Text -> Bool
isMorseCode = Data.Text.all (`Data.Text.elem` ".- ")