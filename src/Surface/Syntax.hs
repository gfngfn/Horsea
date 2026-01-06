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
    TypeExprF (..),
    TypeExprMainF (..),
    ArgForTypeF (..),
    TypeExpr,
    TypeExprMain,
    ArgForType,
    mapMLiteral,
  )
where

import Data.Text (Text)
import Staged.Syntax (Label)
import Util.TokenUtil (Span)
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
  deriving stock (Eq, Show, Functor)

data ExprF ann = Expr ann (ExprMainF ann)
  deriving stock (Show, Functor)

data ExprMainF ann
  = Literal (Literal (ExprF ann))
  | Var ([Var], Var)
  | Lam (Maybe (Var, TypeExprF ann)) (Maybe Label) (Var, TypeExprF ann) (ExprF ann)
  | App (ExprF ann) (Maybe Label) (ExprF ann)
  | LetIn Var [LamBinderF ann] (ExprF ann) (ExprF ann)
  | LetRecIn Var [LamBinderF ann] (TypeExprF ann) (ExprF ann) (ExprF ann)
  | LetTupleIn Var Var (ExprF ann) (ExprF ann)
  | LetOpenIn Var (ExprF ann)
  | Sequential (ExprF ann) (ExprF ann)
  | Tuple (ExprF ann) (ExprF ann)
  | IfThenElse (ExprF ann) (ExprF ann) (ExprF ann)
  | As (ExprF ann) (TypeExprF ann)
  | LamOpt (Var, TypeExprF ann) (ExprF ann)
  | AppOptGiven (ExprF ann) (ExprF ann)
  | AppOptOmitted (ExprF ann)
  deriving stock (Show, Functor)

data LamBinderF ann
  = MandatoryBinder (Maybe Label) (Var, TypeExprF ann)
  | ImplicitBinder (Var, TypeExprF ann)
  deriving stock (Show, Functor)

type Expr = ExprF Span

type ExprMain = ExprMainF Span

type LamBinder = LamBinderF Span

type TypeName = Text

data TypeExprF ann = TypeExpr ann (TypeExprMainF ann)
  deriving stock (Show, Functor)

data TypeExprMainF ann
  = TyName TypeName [ArgForTypeF ann]
  | TyArrow (Maybe Label) (Maybe Var, TypeExprF ann) (TypeExprF ann)
  | TyOptArrow (Var, TypeExprF ann) (TypeExprF ann)
  | TyProduct (TypeExprF ann) (TypeExprF ann)
  deriving stock (Show, Functor)

data ArgForTypeF ann
  = ExprArg (ExprF ann)
  | TypeArg (TypeExprF ann)
  deriving stock (Show, Functor)

type TypeExpr = TypeExprF Span

type TypeExprMain = TypeExprMainF Span

type ArgForType = ArgForTypeF Span

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
