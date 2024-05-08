module DitsAndDahs.Bot (convertText) where

import Data.Text (Text, all, elem)
import DitsAndDahs.MorseCode (morseCodeToText, textToMorseCode)

convertText :: Text -> Text
convertText text
  | isMorseCode text = morseCodeToText text
  | otherwise = textToMorseCode text

isMorseCode :: Text -> Bool
isMorseCode = Data.Text.all (`Data.Text.elem` ".-/ ")