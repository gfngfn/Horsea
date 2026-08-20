module Surface.BindingTime.AnalysisError
  ( AnalysisError (..),
  )
where

import Common.LocationInFile (SpanInFile)
import Data.List.TwoOrMore (TwoOrMore)
import Data.Map (Map)
import Staged.Core (ConstructorName, Label)
import Surface.BindingTime.Core
import Surface.Syntax (ModuleName, TypeName, Var)
import Prelude

data AnalysisError
  = InvalidSyntaxAsExpr SpanInFile
  | InvalidSyntaxAsTypeExpr SpanInFile
  | UnboundVar SpanInFile [ModuleName] Var
  | UnboundConstructor SpanInFile [ModuleName] ConstructorName
  | UnboundModule SpanInFile ModuleName
  | NotAFunction SpanInFile BIType
  | NotAnOptFunction SpanInFile BIType
  | NotABase SpanInFile BIType
  | BindingTimeContradiction SpanInFile
  | BITypeContradiction SpanInFile BIType BIType BIType BIType
  | BITypeInclusionLeft SpanInFile BIType BIType BITypeVar BIType
  | BITypeInclusionRight SpanInFile BIType BIType BIType BITypeVar
  | UnknownTypeOrInvalidArity SpanInFile [ModuleName] TypeName Int
  | NotATuple SpanInFile BIType
  | TupleLengthMismatch SpanInFile (TwoOrMore Var) (TwoOrMore BIType)
  | NotARecord SpanInFile BIType
  | NoRecordField SpanInFile Label (Map Label BIType)
  | LetRecParamsCannotStartWithImplicit SpanInFile
  | LetRecRequiresNonEmptyParams SpanInFile
  | NoOmissibleParameter SpanInFile Label
  deriving stock (Show)
