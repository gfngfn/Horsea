module Surface.BindingTime.Stager
  ( BCExprF,
    BCExprMainF,
    BCLamBinderF,
    BCTypeExprF,
    BCTypeExprMainF,
    BCArgForTypeF,
    stageExpr0,
  )
where

import Staged.SrcSyntax qualified as Staged
import Surface.BindingTime.Core
import Surface.Syntax
import Prelude

type BCExprF ann = ExprF (BindingTimeConst, ann)

type BCExprMainF ann = ExprMainF (BindingTimeConst, ann)

type BCLamBinderF ann = LamBinderF (BindingTimeConst, ann)

type BCTypeExprF ann = TypeExprF (BindingTimeConst, ann)

type BCTypeExprMainF ann = TypeExprMainF (BindingTimeConst, ann)

type BCArgForTypeF ann = ArgForTypeF (BindingTimeConst, ann)

stageExpr0 :: (Show ann) => BCExprF ann -> Staged.ExprF ann
stageExpr0 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Staged.Expr ann (stageExpr0Main exprMain)
    BT1 -> Staged.Expr ann (Staged.Bracket (Staged.Expr ann (stageExpr1Main exprMain)))

stageExpr0Main :: (Show ann) => BCExprMainF ann -> Staged.ExprMainF ann
stageExpr0Main = \case
  Literal lit ->
    Staged.Literal (convertLiteral stageExpr0 lit)
  Var (ms, x) ->
    Staged.Var (ms, x)
  Lam Nothing labelOpt (x, tye1) e2 ->
    Staged.Lam Nothing labelOpt (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  Lam (Just (f, tyeRec)) labelOpt (x, tye1) e2 ->
    Staged.Lam (Just (f, stageTypeExpr0 tyeRec)) labelOpt (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  App e1 labelOpt e2 ->
    Staged.App (stageExpr0 e1) labelOpt (stageExpr0 e2)
  LetIn _x (_ : _) _e1 _e2 ->
    error "Bug: Stager.stageExpr0Main, non-empty parameter sequence"
  LetIn x [] e1 e2 ->
    Staged.LetIn x [] (stageExpr0 e1) (stageExpr0 e2)
  LetRecIn _x _params _tye _e1 _e2 ->
    error "Bug: Stager.stageExpr0Main, LetRecIn"
  LetTupleIn xL xR e1 e2 ->
    Staged.LetTupleIn xL xR (stageExpr0 e1) (stageExpr0 e2)
  LetOpenIn m e ->
    Staged.LetOpenIn m (stageExpr0 e)
  Sequential e1 e2 ->
    Staged.Sequential (stageExpr0 e1) (stageExpr0 e2)
  Tuple e1 e2 ->
    Staged.Tuple (stageExpr0 e1) (stageExpr0 e2)
  IfThenElse e0 e1 e2 ->
    Staged.IfThenElse (stageExpr0 e0) (stageExpr0 e1) (stageExpr0 e2)
  As e1 tye2 ->
    Staged.As (stageExpr0 e1) (stageTypeExpr0 tye2)
  LamOpt (x, tye1) e2 ->
    Staged.LamOpt (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  AppOptGiven e1 e2 ->
    Staged.AppOptGiven (stageExpr0 e1) (stageExpr0 e2)
  AppOptOmitted e1 ->
    Staged.AppOptOmitted (stageExpr0 e1)

stageExpr1 :: (Show ann) => BCExprF ann -> Staged.ExprF ann
stageExpr1 (Expr (btc, ann) exprMain) =
  case btc of
    BT0 -> Staged.Expr ann (Staged.Escape (Staged.Expr ann (stageExpr0Main exprMain)))
    BT1 -> Staged.Expr ann (stageExpr1Main exprMain)

stageExpr1Main :: (Show ann) => BCExprMainF ann -> Staged.ExprMainF ann
stageExpr1Main = \case
  Literal lit ->
    Staged.Literal (convertLiteral stageExpr1 lit)
  Var (ms, x) ->
    Staged.Var (ms, x)
  Lam Nothing labelOpt (x, tye1) e2 ->
    Staged.Lam Nothing labelOpt (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  Lam (Just (f, tyeRec)) labelOpt (x, tye1) e2 ->
    Staged.Lam (Just (f, stageTypeExpr1 tyeRec)) labelOpt (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  App e1 labelOpt e2 ->
    Staged.App (stageExpr1 e1) labelOpt (stageExpr1 e2)
  LetIn _x (_ : _) _e1 _e2 ->
    error "Bug: Stager.stageExpr0Main, non-empty parameter sequence"
  LetIn x [] e1 e2 ->
    Staged.LetIn x [] (stageExpr1 e1) (stageExpr1 e2)
  LetRecIn _x _params _tye _e1 _e2 ->
    error "Bug: Stager.stageExpr0Main, LetRecIn"
  LetTupleIn xL xR e1 e2 ->
    Staged.LetTupleIn xL xR (stageExpr1 e1) (stageExpr1 e2)
  LetOpenIn m e ->
    Staged.LetOpenIn m (stageExpr1 e)
  Sequential e1 e2 ->
    Staged.Sequential (stageExpr1 e1) (stageExpr1 e2)
  Tuple e1 e2 ->
    Staged.Tuple (stageExpr1 e1) (stageExpr1 e2)
  IfThenElse e0 e1 e2 ->
    Staged.IfThenElse (stageExpr1 e0) (stageExpr1 e1) (stageExpr1 e2)
  As e1 tye2 ->
    Staged.As (stageExpr1 e1) (stageTypeExpr1 tye2)
  LamOpt (_x, _tye1) _e2 ->
    error "bug: stageExpr1Main, LamOpt"
  AppOptGiven _e1 _e2 ->
    error "bug: stageExpr1Main, AppOptGiven"
  AppOptOmitted _e1 ->
    error "bug: stageExpr1Main, AppOptOmitted"

stageTypeExpr0 :: (Show ann) => BCTypeExprF ann -> Staged.TypeExprF ann
stageTypeExpr0 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT1 -> Staged.TypeExpr ann (Staged.TyCode (Staged.TypeExpr ann (stageTypeExpr1Main typeExprMain)))
    BT0 -> Staged.TypeExpr ann (stageTypeExpr0Main typeExprMain)

stageTypeExpr0Main :: (Show ann) => BCTypeExprMainF ann -> Staged.TypeExprMainF ann
stageTypeExpr0Main = \case
  TyName tyName args ->
    -- TODO: check that `ExprArg` only contains literals
    Staged.TyName tyName $
      map
        ( \case
            TypeArg tye -> Staged.TypeArg (stageTypeExpr0 tye)
            ExprArg e -> Staged.ExprArgNormal (stageExpr0 e)
        )
        args
  TyArrow labelOpt (xOpt, tye1) tye2 ->
    Staged.TyArrow labelOpt (xOpt, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)
  TyOptArrow (x, tye1) tye2 ->
    Staged.TyOptArrow (x, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)
  TyProduct tye1 tye2 ->
    Staged.TyProduct (stageTypeExpr0 tye1) (stageTypeExpr0 tye2)

stageTypeExpr1 :: (Show ann) => BCTypeExprF ann -> Staged.TypeExprF ann
stageTypeExpr1 (TypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT0 -> error $ "bug: stageTypeExpr1, BT0; " ++ show typeExprMain
    BT1 -> Staged.TypeExpr ann (stageTypeExpr1Main typeExprMain)

stageTypeExpr1Main :: (Show ann) => BCTypeExprMainF ann -> Staged.TypeExprMainF ann
stageTypeExpr1Main = \case
  TyName tyName args -> Staged.TyName tyName (map stageArgForType1 args)
  TyArrow labelOpt (_xOpt, tye1) tye2 -> Staged.TyArrow labelOpt (Nothing, stageTypeExpr1 tye1) (stageTypeExpr1 tye2)
  TyOptArrow (_x, _tye1) _tye2 -> error "bug: stageTypeExpr1Main, TyOptArrow"
  TyProduct tye1 tye2 -> Staged.TyProduct (stageTypeExpr1 tye1) (stageTypeExpr1 tye2)

stageArgForType1 :: (Show ann) => BCArgForTypeF ann -> Staged.ArgForTypeF ann
stageArgForType1 = \case
  ExprArg e -> Staged.ExprArgPersistent (stageExpr0 e)
  TypeArg tye -> Staged.TypeArg (stageTypeExpr1 tye)

convertLiteral :: (se -> le) -> Literal se -> Staged.Literal le
convertLiteral conv = \case
  LitInt n -> Staged.LitInt n
  LitFloat r -> Staged.LitFloat r
  LitUnit -> Staged.LitUnit
  LitBool b -> Staged.LitBool b
  LitString t -> Staged.LitString t
  LitList es -> Staged.LitList (map conv es)
  LitVec ns -> Staged.LitVec ns
  LitMat nss -> Staged.LitMat nss
