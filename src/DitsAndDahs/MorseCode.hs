module DitsAndDahs.MorseCode
  ( toMorseCode,
    fromMorseCode,
  )
where

import Data.Char (toUpper)
import Data.Text (Text, concatMap, pack, words)
import Prelude hiding (concatMap, words)

toMorseCode :: Text -> Text
toMorseCode = concatMap (charToMorse . toUpper)

fromMorseCode :: Text -> Text
fromMorseCode = pack . map morseToChar . words

charToMorse :: Char -> Text
charToMorse 'A' = ".-"
charToMorse 'B' = "-..."
charToMorse 'C' = "-.-."
charToMorse 'D' = "-.."
charToMorse 'E' = "."
charToMorse 'F' = "..-."
charToMorse 'G' = "--."
charToMorse 'H' = "...."
charToMorse 'I' = ".."
charToMorse 'J' = ".---"
charToMorse 'K' = "-.-"
charToMorse 'L' = ".-.."
charToMorse 'M' = "--"
charToMorse 'N' = "-."
charToMorse 'O' = "---"
charToMorse 'P' = ".--."
charToMorse 'Q' = "--.-"
charToMorse 'R' = ".-."
charToMorse 'S' = "..."
charToMorse 'T' = "-"
charToMorse 'U' = "..-"
charToMorse 'V' = "...-"
charToMorse 'W' = ".--"
charToMorse 'X' = "-..-"
charToMorse 'Y' = "-.--"
charToMorse 'Z' = "--.."
charToMorse '0' = "-----"
charToMorse '1' = ".----"
charToMorse '2' = "..---"
charToMorse '3' = "...--"
charToMorse '4' = "....-"
charToMorse '5' = "....."
charToMorse '6' = "-...."
charToMorse '7' = "--..."
charToMorse '8' = "---.."
charToMorse '9' = "----."
charToMorse ' ' = " "
charToMorse _ = ""

morseToChar :: Text -> Char
morseToChar "-..." = 'B'
morseToChar ".-" = 'A'
morseToChar "-.-." = 'C'
morseToChar "-.." = 'D'
morseToChar "." = 'E'
morseToChar "..-." = 'F'
morseToChar "--." = 'G'
morseToChar "...." = 'H'
morseToChar ".." = 'I'
morseToChar ".---" = 'J'
morseToChar "-.-" = 'K'
morseToChar ".-.." = 'L'
morseToChar "--" = 'M'
morseToChar "-." = 'N'
morseToChar "---" = 'O'
morseToChar ".--." = 'P'
morseToChar "--.-" = 'Q'
morseToChar ".-." = 'R'
morseToChar "..." = 'S'
morseToChar "-" = 'T'
morseToChar "..-" = 'U'
morseToChar "...-" = 'V'
morseToChar ".--" = 'W'
morseToChar "-..-" = 'X'
morseToChar "-.--" = 'Y'
morseToChar "--.." = 'Z'
morseToChar "-----" = '0'
morseToChar ".----" = '1'
morseToChar "..---" = '2'
morseToChar "...--" = '3'
morseToChar "....-" = '4'
morseToChar "....." = '5'
morseToChar "-...." = '6'
morseToChar "--..." = '7'
morseToChar "---.." = '8'
morseToChar "----." = '9'
morseToChar _ = ' '