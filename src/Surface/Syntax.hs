module Surface.Syntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    RecordFieldF (..),
    LamBinderF (..),
    Expr,
    ExprMain,
    RecordField,
    LamBinder,
    TypeName,
    TypeExprF,
    TypeExpr,
    ModuleName,
  )
where

import Common.TokenUtil (Span)
import Data.List.NonEmpty (NonEmpty)
import Data.List.TwoOrMore (TwoOrMore)
import Data.Text (Text)
import Staged.Core (ConstructorName, Label)
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
  deriving stock (Eq, Show, Functor, Foldable, Traversable)

data ExprF ann = Expr ann (ExprMainF ann)
  deriving stock (Show, Functor)

data ExprMainF ann
  = Literal (Literal (ExprF ann))
  | Var ([ModuleName], Var)
  | Constructor ([ModuleName], ConstructorName)
  | Lam (Maybe (Var, TypeExprF ann)) (Maybe Label) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (Maybe Label) (ExprF ann)
  | LetIn Var [LamBinderF ann] (Maybe (TypeExprF ann)) (ExprF ann) (ExprF ann)
  | LetRecIn Var [LamBinderF ann] (TypeExprF ann) (ExprF ann) (ExprF ann)
  | LetTupleIn (TwoOrMore Var) (ExprF ann) (ExprF ann)
  | LetOpenIn Var (ExprF ann)
  | Sequential (ExprF ann) (ExprF ann)
  | Tuple (TwoOrMore (ExprF ann))
  | Record [(Label, RecordFieldF ann)]
  | FieldProj (ExprF ann) Label
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | As (ExprF ann) (TypeExprF ann)
  | LamOms Label (Var, TypeExprF ann) (ExprF ann)
  | AppOms (ExprF ann) Label (ExprF ann)
  | LamInf (Var, TypeExprF ann) (ExprF ann)
  | AppInfGiven (ExprF ann) (ExprF ann)
  | AppInfOmitted (ExprF ann)
  | TyArrow (Maybe Label) (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyOmsArrow Text (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyInfArrow (Var, TypeExprF ann) (TypeExprF ann)
  | TyRefinement Var (TypeExprF ann) (ExprF ann)
  | Product (TypeExprF ann) (NonEmpty ((ann, Var), TypeExprF ann))
  deriving stock (Show, Functor)

data RecordFieldF ann
  = RecordFieldEqual (ExprF ann)
  | RecordFieldColon (TypeExprF ann)
  deriving stock (Show, Functor)

data LamBinderF ann
  = MandatoryBinder (Maybe Label) (Var, TypeExprF ann)
  | OmissibleBinder Label (Var, TypeExprF ann)
  | InferableBinder (Var, TypeExprF ann)
  deriving stock (Show, Functor)

type Expr = ExprF Span

type ExprMain = ExprMainF Span

type RecordField = RecordFieldF Span

type LamBinder = LamBinderF Span

type TypeName = Text

type TypeExprF = ExprF

type TypeExpr = TypeExprF Span

type ModuleName = Text
