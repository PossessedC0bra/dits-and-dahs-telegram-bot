module Data.Aeson.Utilities.Options (module Data.Aeson.Utilities.Options) where

import Data.Aeson (Options (..), camelTo2, defaultOptions)

reservedKeywordFieldRenameOptions :: Options
reservedKeywordFieldRenameOptions = defaultOptions {fieldLabelModifier = reservedKeywordFieldRename}

reservedKeywordFieldRename :: String -> String
reservedKeywordFieldRename "type_" = "type"
reservedKeywordFieldRename f = f

constructorCamelToSnakeCaseOptions :: Options
constructorCamelToSnakeCaseOptions = defaultOptions {constructorTagModifier = camelToSnake}

camelToSnake :: String -> String
camelToSnake = camelTo2 '_'