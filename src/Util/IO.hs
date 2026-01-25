module Util.IO
  ( readFileEither,
  )
where

import Control.Exception (IOException, catch)
import Data.Text (Text)
import Data.Text.IO qualified as TextIO
import Prelude

readFileEither :: FilePath -> IO (Either String Text)
readFileEither path =
  (Right <$> TextIO.readFile path) `catch` (\e -> pure (Left (show (e :: IOException))))
