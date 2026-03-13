module Surface.Syntax
  ( Var,
    Literal (..),
    ExprF (..),
    ExprMainF (..),
    LamBinderF (..),
    Expr,
    ExprMain,
    LamBinder,
    TypeName,
    TypeExprF,
    TypeExpr,
    mapMLiteral,
  )
where

import Data.List.NonEmpty (NonEmpty)
import Data.List.TwoOrMore (TwoOrMore)
import Data.Text (Text)
import Staged.Core
import Util.TokenUtil (Located, Span)
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
  | Var ([Var], Var)
  | Constructor ([Var], Var)
  | Lam (Maybe (Var, TypeExprF ann)) (Maybe Label) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (Maybe Label) (ExprF ann)
  | LetIn Var [LamBinderF ann] (Maybe (TypeExprF ann)) (ExprF ann) (ExprF ann)
  | LetRecIn Var [LamBinderF ann] (TypeExprF ann) (ExprF ann) (ExprF ann)
  | LetTupleIn (TwoOrMore Var) (ExprF ann) (ExprF ann)
  | LetOpenIn Var (ExprF ann)
  | Sequential (ExprF ann) (ExprF ann)
  | Tuple (TwoOrMore (ExprF ann))
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | As (ExprF ann) (TypeExprF ann)
  | LamImp (Var, TypeExprF ann) (ExprF ann)
  | AppImpGiven (ExprF ann) (ExprF ann)
  | AppImpOmitted (ExprF ann)
  | TyArrow (Maybe Label) (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyImpArrow (Var, TypeExprF ann) (TypeExprF ann)
  | TyRefinement Var (TypeExprF ann) (ExprF ann)
  | Product (TypeExprF ann) (NonEmpty (Located Var, TypeExprF ann))
  deriving stock (Show, Functor)

data LamBinderF ann
  = MandatoryBinder (Maybe Label) (Var, TypeExprF ann)
  | ImplicitBinder (Var, TypeExprF ann)
  deriving stock (Show, Functor)

type Expr = ExprF Span

type ExprMain = ExprMainF Span

type LamBinder = LamBinderF Span

type TypeName = Text

type TypeExprF = ExprF

type TypeExpr = TypeExprF Span

mapMLiteral :: (Monad m) => (a -> m b) -> Literal a -> m (Literal b)
mapMLiteral f = \case
  LitInt n -> pure $ LitInt n
  LitFloat r -> pure $ LitFloat r
  LitUnit -> pure LitUnit
  LitBool b -> pure $ LitBool b
  LitString t -> pure $ LitString t
  LitList es -> LitList <$> mapM f es
  LitVec ns -> pure $ LitVec ns
  LitMat nss -> pure $ LitMat nss
