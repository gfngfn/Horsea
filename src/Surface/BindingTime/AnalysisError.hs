module Surface.BindingTime.AnalysisError
  ( AnalysisError (..),
  )
where

import Surface.BindingTime.Core
import Surface.Syntax
import Util.LocationInFile (SpanInFile)
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
  | BITypeInclusionLeft SpanInFile BIType BIType BITypeVar BIType
  | BITypeInclusionRight SpanInFile BIType BIType BIType BITypeVar
  | UnknownTypeOrInvalidArgs SpanInFile TypeName [ArgForType]
  | NotATuple SpanInFile BIType
  | LetRecParamsCannotStartWithImplicit SpanInFile
  | LetRecRequiresNonEmptyParams SpanInFile
  deriving stock (Show)
