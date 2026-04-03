module Staged.TypeError
  ( TypeErrorF (..),
    ConditionalMergeErrorF (..),
    UnsupportedF (..),
    TypeError,
    ConditionalMergeError,
  )
where

import Common.LocationInFile (SpanInFile)
import Data.List.NonEmpty (NonEmpty)
import Data.List.TwoOrMore (TwoOrMore)
import Data.Tensor.Matrix qualified as Matrix
import Data.Text (Text)
import Staged.Core
import Staged.SrcSyntax
import Staged.Syntax
import Prelude

data TypeErrorF sv
  = Unsupported SpanInFile (UnsupportedF sv)
  | InvalidSyntaxAsExpr SpanInFile
  | InvalidSyntaxAsPattern SpanInFile
  | InvalidSyntaxAsTypeExpr SpanInFile
  | UnboundVar SpanInFile [ModuleName] Var
  | UnboundTypeVar SpanInFile TypeVar
  | UnboundModule SpanInFile ModuleName
  | NotAStage0Var SpanInFile Var
  | NotAStage1Var SpanInFile Var
  | UnboundConstructor SpanInFile [ModuleName] ConstructorName
  | UnboundConstructorOrInvalidArity SpanInFile [ModuleName] ConstructorName Int
  | UnknownTypeOrInvalidArityAtStage0 SpanInFile [Var] TypeName Int
  | UnknownTypeOrInvalidArityAtStage1 SpanInFile [Var] TypeName Int
  | NotAnIntLitArgAtStage0 SpanInFile (Ass0ExprF sv)
  | NotAnIntListLitArgAtStage0 SpanInFile (Ass0ExprF sv)
  | TypeContradictionAtStage0 SpanInFile (Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | TypeContradictionAtStage1 SpanInFile (Ass1TypeExprF sv) (Ass1TypeExprF sv)
  | NotABoolTypeForStage0 SpanInFile (Ass0TypeExprF sv)
  | NotABoolTypeForStage1 SpanInFile (Ass1TypeExprF sv)
  | NotAUnitTypeForStage0 SpanInFile (Ass0TypeExprF sv)
  | NotAUnitTypeForStage1 SpanInFile (Ass1TypeExprF sv)
  | NotACodeType SpanInFile (Ass0TypeExprF sv)
  | CannotUseEscapeAtStage0 SpanInFile
  | CannotUseBracketAtStage1 SpanInFile
  | CannotUseLamInfAtStage1 SpanInFile
  | CannotUseAppInfGivenAtStage1 SpanInFile
  | CannotUseAppInfOmittedAtStage1 SpanInFile
  | FunctionTypeCannotBeDependentAtStage1 SpanInFile Var
  | CannotUseCodeTypeAtStage1 SpanInFile
  | CannotUseInfArrowTypeAtStage1 SpanInFile
  | CannotUseRefinementTypeAtStage1 SpanInFile
  | CannotUsePersistent SpanInFile
  | CannotUseNormalArgAtStage1 SpanInFile
  | VarOccursFreelyInAss0Type SpanInFile Var (ResultF Ass0TypeExprF sv)
  | VarOccursFreelyInAss1Type SpanInFile Var (ResultF Ass1TypeExprF sv)
  | InvalidMatrixLiteral SpanInFile (Matrix.ConstructionError Int)
  | CannotMergeTypesByConditional0 SpanInFile (NonEmpty (Ass0PatternF sv, Ass0TypeExprF sv)) (ConditionalMergeErrorF sv)
  | CannotMergeTypesByConditional1 SpanInFile (NonEmpty (Ass0PatternF sv, Ass1TypeExprF sv)) (ConditionalMergeErrorF sv)
  | CannotMergeResultsByConditionals SpanInFile (NonEmpty (Ass0PatternF sv, ResultF Ass0TypeExprF sv))
  | CannotApplyLiteral SpanInFile
  | CannotInstantiateGuidedByAppContext0 SpanInFile (AppContextF sv) (Ass0TypeExprF sv)
  | CannotInstantiateGuidedByAppContext1 SpanInFile (AppContextF sv) (Ass1TypeExprF sv)
  | CannotInferImplicit SpanInFile (AssVarF sv) (Ass0TypeExprF sv) (AppContextF sv)
  | CannotInferTypeVariableInstance0 SpanInFile AssTypeVar (AppContextF sv) (Ass0TypeExprF sv)
  | CannotInferTypeVariableInstance1 SpanInFile AssTypeVar (AppContextF sv) (Ass1TypeExprF sv)
  | CannotInstantiateTypeVariableGuidedByAssertion0 SpanInFile AssTypeVar (Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | Stage1IfThenElseRestrictedToEmptyContext SpanInFile (AppContextF sv)
  | Stage1CaseRestrictedToEmptyContext SpanInFile (AppContextF sv)
  | BindingOverwritten SpanInFile Var
  | UnknownExternalName SpanInFile Text
  | InvalidPersistentType SpanInFile (Ass0TypeExprF sv)
  | InvalidTypeForRefinement SpanInFile (Ass0TypeExprF sv)
  | NoBuiltInNameInExternal SpanInFile
  | CannotApplyTuple SpanInFile
  | NotATupleAtStage0 SpanInFile (Ass0TypeExprF sv)
  | NotATupleAtStage1 SpanInFile (Ass1TypeExprF sv)
  | LetRecParamsCannotStartWithImplicit SpanInFile
  | LetRecRequiresNonEmptyParams SpanInFile
  | CannotSynthesizeTypeFromExpr SpanInFile
  | CannotForceType0 SpanInFile (Ass0TypeExprF sv)
  | CannotForceType1 SpanInFile (Ass1TypeExprF sv)
  | CannotForceTypeOnPattern0 SpanInFile (Ass0TypeExprF sv)
  | CannotForceTypeOnPattern1 SpanInFile (Ass1TypeExprF sv)
  | ApplicationLabelMismatch SpanInFile (AppContextF sv) (Maybe Label) (Maybe Label)
  | NotAStage0TypeVar SpanInFile TypeVar
  | NotAStage1TypeVar SpanInFile TypeVar
  | LetTupleLengthMismatch0 SpanInFile (TwoOrMore Var) (TwoOrMore (Ass0TypeExprF sv))
  | LetTupleLengthMismatch1 SpanInFile (TwoOrMore Var) (TwoOrMore (Ass1TypeExprF sv))
  | NonMaybeAnnotForLamOms0 SpanInFile (Ass0TypeExprF sv)
  | NonMaybeAnnotForLamOms1 SpanInFile (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

data ConditionalMergeErrorF sv
  = CannotMerge0 (NonEmpty (Ass0PatternF sv, Ass0TypeExprF sv))
  | CannotMerge1 (NonEmpty (Ass0PatternF sv, Ass1TypeExprF sv))
  deriving stock (Eq, Show, Functor)

data UnsupportedF sv
  = CannotBindPersistentValue Var
  | HigherRankPolymorphism (Ass0TypeExprF sv) AssTypeVar (Ass0TypeExprF sv)
  | AsWithArguments (AppContextF sv)
  | LamWithArguments (AppContextF sv)
  | LamOmsWithArguments (AppContextF sv)
  | LamInfWithArguments (AppContextF sv)
  | PersistentFunWithOms
  deriving stock (Eq, Show, Functor)

type TypeError = TypeErrorF StaticVar

type ConditionalMergeError = ConditionalMergeErrorF StaticVar
