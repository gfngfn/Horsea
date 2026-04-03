module Surface.BindingTime.AnalysisError
  ( AnalysisError (..),
  )
where

import Common.LocationInFile (SpanInFile)
import Data.List.TwoOrMore (TwoOrMore)
import Staged.Core (Label)
import Surface.BindingTime.Core
import Surface.Syntax
import Prelude

data AnalysisError
  = InvalidSyntaxAsExpr SpanInFile
  | InvalidSyntaxAsTypeExpr SpanInFile
  | UnboundVar SpanInFile [Var] Var
  | NotAVal SpanInFile [Var] Var
  | NotAModule SpanInFile Var
  | NotAFunction SpanInFile BIType
  | NotAnOptFunction SpanInFile BIType
  | NotABase SpanInFile BIType
  | BindingTimeContradiction SpanInFile
  | BITypeContradiction SpanInFile BIType BIType BIType BIType
  | BITypeInclusionLeft SpanInFile BIType BIType BITypeVar BIType
  | BITypeInclusionRight SpanInFile BIType BIType BIType BITypeVar
  | UnknownTypeOrInvalidArgs SpanInFile TypeName [Expr]
  | NotATuple SpanInFile BIType
  | TupleLengthMismatch SpanInFile (TwoOrMore Var) (TwoOrMore BIType)
  | LetRecParamsCannotStartWithImplicit SpanInFile
  | LetRecRequiresNonEmptyParams SpanInFile
  | NoOmissibleParameter SpanInFile Label
  deriving stock (Show)
