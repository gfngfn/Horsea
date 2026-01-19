module Surface.BindingTime.AnalysisError
  ( AnalysisError (..),
  )
where

import Surface.BindingTime.Core
import Surface.Syntax
import Util.LocationInFile (SpanInFile)
import Util.TokenUtil
import Prelude

data AnalysisError
  = UnboundVar SpanInFile [Var] Var
  | NotAVal SpanInFile [Var] Var
  | NotAModule SpanInFile Var
  | NotAFunction SpanInFile BIType
  | NotAnOptFunction SpanInFile BIType
  | NotABase SpanInFile BIType
  | BindingTimeContradiction SpanInFile
  | BITypeContradiction SpanInFile BIType BIType BIType BIType
  | UnknownTypeOrInvalidArgs SpanInFile TypeName [ArgForTypeF Span]
  | NotATuple SpanInFile BIType
  deriving stock (Show)
