module Staged.SrcSyntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    LamBinderF (..),
    Expr,
    ExprMain,
    LamBinder,
    TypeName,
    TypeVar (..),
    TypeExprF,
    TypeExprMainF,
    TypeExpr,
    TypeExprMain,
    BindF (..),
    BindMainF (..),
    Bind,
    BindValF (..),
    BindVal,
    Stage (..),
    ExternalField,
    External,
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
  | Var ([Var], Var) -- A module name chain and a value identifier
  | Constructor ([Var], Text)
  | Lam (Maybe (Var, TypeExprF ann)) (Maybe Label) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (Maybe Label) (ExprF ann)
  | LetIn Var [LamBinderF ann] (Maybe (TypeExprF ann)) (ExprF ann) (ExprF ann)
  | LetRecIn Var [LamBinderF ann] (TypeExprF ann) (ExprF ann) (ExprF ann)
  | LetTupleIn (TwoOrMore Var) (ExprF ann) (ExprF ann)
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | As (ExprF ann) (TypeExprF ann)
  | Bracket (ExprF ann)
  | Escape (ExprF ann)
  | LamImp (Var, TypeExprF ann) (ExprF ann)
  | AppImpGiven (ExprF ann) (ExprF ann)
  | AppImpOmitted (ExprF ann)
  | LetOpenIn Var (ExprF ann)
  | Sequential (ExprF ann) (ExprF ann)
  | Tuple (TwoOrMore (ExprF ann))
  | Product (ExprF ann) (NonEmpty ((ann, Var), ExprF ann))
  | Persistent (ExprF ann)
  | TyVar TypeVar
  | TyArrow (Maybe Text) (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyImpArrow (Var, TypeExprF ann) (TypeExprF ann)
  | TyRefinement Var (TypeExprF ann) (ExprF ann)
  | TyForAll TypeVar (TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

data LamBinderF ann
  = MandatoryBinder (Maybe Label) (Var, TypeExprF ann)
  | ImplicitBinder (Var, TypeExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

type Expr = ExprF Span

type ExprMain = ExprMainF Span

type LamBinder = LamBinderF Span

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
  | BindModule Var [BindF ann]
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

type Bind = BindF Span

data BindValF ann
  = BindValExternal (TypeExprF ann) External
  | BindValNormal (ExprF ann)
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

type BindVal = BindValF Span

data Stage = Stage0 | Stage1 | StagePers
  deriving stock (Eq, Show)

type ExternalField = Text

type External = [(ExternalField, Text)]
