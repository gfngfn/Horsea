module Util.String
  ( snakeToCamel,
    uppercase,
  )
where

import Data.Char qualified as Char
import Data.Text qualified as Text
import Prelude

snakeToCamel :: String -> String
snakeToCamel s = concatMap capitalizeOrRecoverUnderscore $ split '_' s

-- TODO: make this more efficient
split :: Char -> String -> [String]
split c = map Text.unpack . Text.split (== c) . Text.pack

capitalizeOrRecoverUnderscore :: String -> String
capitalizeOrRecoverUnderscore = \case
  chHead : chTail -> Char.toUpper chHead : chTail
  [] -> "_"

uppercase :: String -> String
uppercase = map Char.toUpper
