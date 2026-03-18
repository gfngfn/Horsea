module Common.FrontError
  ( FrontError (..),
  )
where

import Common.ParserUtil (ParseError)
import Prelude

data FrontError
  = FrontLexingError String
  | FrontParseError [ParseError]
  deriving stock (Eq, Show)
