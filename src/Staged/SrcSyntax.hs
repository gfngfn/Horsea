module Staged.SrcSyntax
  ( Var,
    ModuleName,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    RecordFieldF (..),
    LamBinderF (..),
    TypeParamBinderF (..),
    BranchF (..),
    PatternF (..),
    PatternMainF (..),
    Expr,
    ExprMain,
    RecordField,
    LamBinder,
    TypeParamBinder,
    Branch,
    Pattern,
    PatternMain,
    TypeName,
    TypeVar (..),
    TypeExprF,
    TypeExprMainF,
    TypeExpr,
    TypeExprMain,
    BindF (..),
    BindMainF (..),
    BindValF (..),
    TypeDefinitionF (..),
    Stage (..),
    ExternalField,
    External,
    Bind,
    BindVal,
    TypeDefinition,
  )
where

import Common.TokenUtil (Span)
import Data.List.NonEmpty (NonEmpty)
import Data.List.TwoOrMore (TwoOrMore)
import Data.Text (Text)
import Generic.Data (Generic)
import Generic.Data.Orphans ()
import Staged.Core
import Prelude

type Var = Text

type ModuleName = Text

data Literal e
  = LitInt Int
  | LitFloat Double
  | LitUnit
  | LitBool Bool
  | LitString Text
  | LitList [e]
  | LitVec [Int]
  | LitMat [[Int]]
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

-- | The type of ASTs for expressions obtained by parsing source programs.
data ExprF ann = Expr ann (ExprMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data ExprMainF ann
  = Literal (Literal (ExprF ann))
  | Var ([ModuleName], Var)
  | Constructor ([ModuleName], Text)
  | Lam (Maybe (Var, TypeExprF ann)) (Maybe Label) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (Maybe Label) (ExprF ann)
  | LetIn Var [LamBinderF ann] (Maybe (TypeExprF ann)) (ExprF ann) (ExprF ann)
  | LetRecIn Var [LamBinderF ann] (TypeExprF ann) (ExprF ann) (ExprF ann)
  | LetTupleIn (TwoOrMore Var) (ExprF ann) (ExprF ann)
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | Case (ExprF ann) (NonEmpty (BranchF ann))
  | As (ExprF ann) (TypeExprF ann)
  | Bracket (ExprF ann)
  | Escape (ExprF ann)
  | LamOms Label (Var, TypeExprF ann) (ExprF ann)
  | AppOms (ExprF ann) Label (ExprF ann)
  | LamInf (Var, TypeExprF ann) (ExprF ann)
  | AppInfGiven (ExprF ann) (ExprF ann)
  | AppInfOmitted (ExprF ann)
  | LetOpenIn ModuleName (ExprF ann)
  | Sequential (ExprF ann) (ExprF ann)
  | Tuple (TwoOrMore (ExprF ann))
  | Product (ExprF ann) (NonEmpty ((ann, Var), ExprF ann))
  | Record [(Label, RecordFieldF ann)]
  | FieldProj (ExprF ann) Label
  | LamInfType TypeVar (ExprF ann)
  | AppInfType (ExprF ann) (TypeExprF ann)
  | Persistent (ExprF ann)
  | TyVar TypeVar
  | TyArrow (Maybe Text) (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyOmsArrow Text (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyInfArrow (Var, TypeExprF ann) (TypeExprF ann)
  | TyRefinement Var (TypeExprF ann) (ExprF ann)
  | TyForAll TypeVar (TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data RecordFieldF ann
  = RecordFieldEqual (ExprF ann)
  | RecordFieldColon (TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data LamBinderF ann
  = MandatoryBinder (Maybe Label) (Var, TypeExprF ann)
  | OmissibleBinder Label (Var, TypeExprF ann)
  | InferableBinder (Var, TypeExprF ann)
  | TypeBinder TypeVar
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data TypeParamBinderF ann
  = TypeParamTypeBinder TypeVar
  | TypeParamVal0Binder (Var, TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data BranchF ann = Branch (PatternF ann) (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data PatternF ann = Pattern ann (PatternMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data PatternMainF ann
  = PatVar Var
  | PatBool Bool
  | PatListNil
  | PatConstructor ([ModuleName], ConstructorName)
  | PatApp (PatternF ann) (PatternF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

type Expr = ExprF Span

type ExprMain = ExprMainF Span

type RecordField = RecordFieldF Span

type LamBinder = LamBinderF Span

type TypeParamBinder = TypeParamBinderF Span

type Branch = BranchF Span

type Pattern = PatternF Span

type PatternMain = PatternMainF Span

type TypeName = Text

newtype TypeVar = TypeVar Text
  deriving stock (Eq, Show)

type TypeExprF = ExprF

type TypeExprMainF = ExprMainF

type TypeExpr = TypeExprF Span

type TypeExprMain = ExprMainF Span

data BindF ann = Bind ann (BindMainF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data BindMainF ann
  = BindVal Stage Var (BindValF ann)
  | BindType Stage TypeName [TypeParamBinderF ann] (TypeDefinitionF ann)
  | BindModule Var [BindF ann]
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data BindValF ann
  = BindValExternal (TypeExprF ann) External
  | BindValNormal [LamBinderF ann] (Maybe (TypeExprF ann)) (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data TypeDefinitionF ann
  = TypeDefAlias (TypeExprF ann)
  | TypeDefData (NonEmpty ((ConstructorName, ann), [TypeExprF ann]))
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data Stage = Stage0 | Stage1 | StagePers
  deriving stock (Eq, Show)

type ExternalField = Text

type External = [(ExternalField, Text)]

type Bind = BindF Span

type BindVal = BindValF Span

type TypeDefinition = TypeDefinitionF Span
