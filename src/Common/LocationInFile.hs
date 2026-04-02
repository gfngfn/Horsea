module Common.LocationInFile
  ( LocationInFile (..),
    getLocationInFileFromOffset,
    SourceSpec (..),
    SpanInFile (..),
    getSpanInFile,
  )
where

import Common.TokenUtil (Span (..))
import Data.Text (Text)
import Text.Megaparsec (PosState (..), SourcePos (..))
import Text.Megaparsec.Pos qualified as MpPos
import Text.Megaparsec.Stream qualified as MpStream
import Prelude

data LocationInFile = LocationInFile
  { line :: Int,
    column :: Int
  }
  deriving stock (Eq, Show)

getLocationInFileFromOffset :: String -> Text -> Int -> (LocationInFile, Maybe String)
getLocationInFileFromOffset inputFilePath source offset =
  let initialState =
        PosState
          { pstateInput = source,
            pstateOffset = 0,
            pstateSourcePos = MpPos.initialPos inputFilePath,
            pstateTabWidth = MpPos.defaultTabWidth,
            pstateLinePrefix = ""
          }
      (maybeLineText, finalState) = MpStream.reachOffset offset initialState
      PosState {pstateSourcePos = finalPos} = finalState
      SourcePos {sourceLine, sourceColumn} = finalPos
      locInFile =
        LocationInFile
          { line = MpPos.unPos sourceLine,
            column = MpPos.unPos sourceColumn
          }
   in (locInFile, maybeLineText)

data SourceSpec = SourceSpec
  { source :: Text,
    inputFilePath :: String
  }

data SpanInFile = SpanInFile
  { startLocation :: LocationInFile,
    endLocation :: LocationInFile,
    contents :: Maybe String
  }
  deriving stock (Eq, Show)

getSpanInFile :: SourceSpec -> Span -> SpanInFile
getSpanInFile SourceSpec {source, inputFilePath} Span {start, end} =
  SpanInFile {startLocation, endLocation, contents}
  where
    (startLocation, contents) = getLocationInFileFromOffset inputFilePath source start
    (endLocation, _) = getLocationInFileFromOffset inputFilePath source end
