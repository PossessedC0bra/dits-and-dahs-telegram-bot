module Data.Aeson.Utilities.Options (reservedKeywordFieldRenameOptions, constructorCamelToSnakeCaseOptions, constructorLowercaseFirstOptions) where

import Data.Aeson (Options (..), camelTo2, defaultOptions)
import Data.Char (toLower)

reservedKeywordFieldRenameOptions :: Options
reservedKeywordFieldRenameOptions = defaultOptions {fieldLabelModifier = reservedKeywordFieldRename}

reservedKeywordFieldRename :: String -> String
reservedKeywordFieldRename "type_" = "type"
reservedKeywordFieldRename f = f

constructorCamelToSnakeCaseOptions :: Options
constructorCamelToSnakeCaseOptions = defaultOptions {constructorTagModifier = camelToSnake}

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'

constructorLowercaseFirstOptions :: Options
constructorLowercaseFirstOptions = defaultOptions {constructorTagModifier = lowercaseFirstCharacter}

lowercaseFirstCharacter :: String -> String
lowercaseFirstCharacter (x : xs) = toLower x : xs
lowercaseFirstCharacter [] = []