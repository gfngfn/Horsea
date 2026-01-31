module Staged.TypeError
  ( TypeErrorF (..),
    ConditionalMergeErrorF (..),
    TypeError,
    ConditionalMergeError,
  )
where

import Data.Text (Text)
import Staged.Core
import Staged.SrcSyntax
import Staged.Syntax
import Util.LocationInFile (SpanInFile)
import Util.Matrix qualified as Matrix
import Prelude

data TypeErrorF sv
  = UnboundVar SpanInFile [Var] Var
  | UnboundTypeVar SpanInFile TypeVar
  | UnboundModule SpanInFile Var
  | NotAStage0Var SpanInFile Var
  | NotAStage1Var SpanInFile Var
  | UnknownTypeOrInvalidArityAtStage0 SpanInFile TypeName Int
  | UnknownTypeOrInvalidArityAtStage1 SpanInFile TypeName Int
  | NotAnIntLitArgAtStage0 SpanInFile (Ass0ExprF sv)
  | NotAnIntListLitArgAtStage0 SpanInFile (Ass0ExprF sv)
  | NotAValueArg SpanInFile
  | NotATypeArg SpanInFile
  | TypeContradictionAtStage0 SpanInFile (Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | TypeContradictionAtStage1 SpanInFile (Ass1TypeExprF sv) (Ass1TypeExprF sv)
  | NotABoolTypeForStage0 SpanInFile (Ass0TypeExprF sv)
  | NotABoolTypeForStage1 SpanInFile (Ass1TypeExprF sv)
  | NotAUnitTypeForStage0 SpanInFile (Ass0TypeExprF sv)
  | NotAUnitTypeForStage1 SpanInFile (Ass1TypeExprF sv)
  | NotACodeType SpanInFile (Ass0TypeExprF sv)
  | CannotUseEscapeAtStage0 SpanInFile
  | CannotUseBracketAtStage1 SpanInFile
  | CannotUseLamOptAtStage1 SpanInFile
  | CannotUseAppOptGivenAtStage1 SpanInFile
  | CannotUseAppOptOmittedAtStage1 SpanInFile
  | FunctionTypeCannotBeDependentAtStage1 SpanInFile Var
  | CannotUseCodeTypeAtStage1 SpanInFile
  | CannotUseOptArrowTypeAtStage1 SpanInFile
  | CannotUseRefinementTypeAtStage1 SpanInFile
  | CannotUsePersistentArgAtStage0 SpanInFile
  | CannotUseNormalArgAtStage1 SpanInFile
  | CannotUseTypeVarAtStage1 SpanInFile
  | VarOccursFreelyInAss0Type SpanInFile Var (ResultF Ass0TypeExprF sv)
  | VarOccursFreelyInAss1Type SpanInFile Var (ResultF Ass1TypeExprF sv)
  | InvalidMatrixLiteral SpanInFile Matrix.ConstructionError
  | CannotMergeTypesByConditional0 SpanInFile (Ass0TypeExprF sv) (Ass0TypeExprF sv) (ConditionalMergeErrorF sv)
  | CannotMergeTypesByConditional1 SpanInFile (Ass1TypeExprF sv) (Ass1TypeExprF sv) (ConditionalMergeErrorF sv)
  | CannotMergeResultsByConditionals SpanInFile (ResultF Ass0TypeExprF sv) (ResultF Ass0TypeExprF sv)
  | CannotApplyLiteral SpanInFile
  | CannotInstantiateGuidedByAppContext0 SpanInFile (AppContextF sv) (Ass0TypeExprF sv)
  | CannotInstantiateGuidedByAppContext1 SpanInFile (AppContextF sv) (Ass1TypeExprF sv)
  | CannotInferOptional SpanInFile (AssVarF sv) (Ass0TypeExprF sv) (AppContextF sv)
  | Stage1IfThenElseRestrictedToEmptyContext SpanInFile (AppContextF sv)
  | BindingOverwritten SpanInFile Var
  | UnknownExternalName SpanInFile Text
  | InvalidPersistentType SpanInFile (Ass0TypeExprF sv)
  | InvalidTypeForRefinement SpanInFile (Ass0TypeExprF sv)
  | NoBuiltInNameInExternal SpanInFile
  | CannotApplyTuple SpanInFile
  | NotATupleAtStage0 SpanInFile (Ass0TypeExprF sv)
  | NotATupleAtStage1 SpanInFile (Ass1TypeExprF sv)
  | LetRecParamsCannotStartWithOptional SpanInFile
  | LetRecRequiresNonEmptyParams SpanInFile
  | CannotSynthesizeTypeFromExpr SpanInFile
  | CannotForceType SpanInFile (Ass0TypeExprF sv)
  | ApplicationLabelMismatch SpanInFile (AppContextF sv) (Maybe Label) (Maybe Label)
  deriving stock (Eq, Show, Functor)

data ConditionalMergeErrorF sv
  = CannotMerge0 (Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | CannotMerge1 (Ass1TypeExprF sv) (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

type TypeError = TypeErrorF StaticVar

type ConditionalMergeError = ConditionalMergeErrorF StaticVar
