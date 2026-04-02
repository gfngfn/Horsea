module Surface.BindingTime.Stager
  ( BCExprF,
    BCExprMainF,
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

type BCExprF ann = BExprF ann BindingTimeConst

type BCExprMainF ann = BExprMainF ann BindingTimeConst

type BCTypeExprF ann = BTypeExprF ann BindingTimeConst

type BCTypeExprMainF ann = BTypeExprMainF ann BindingTimeConst

type BCArgForTypeF ann = BArgForTypeF ann BindingTimeConst

stageExpr0 :: (Show ann) => BCExprF ann -> Staged.ExprF ann
stageExpr0 (BExpr (btc, ann) exprMain) =
  case btc of
    BT0 -> Staged.Expr ann (stageExpr0Main exprMain)
    BT1 -> Staged.Expr ann (Staged.Bracket (Staged.Expr ann (stageExpr1Main exprMain)))

stageExpr0Main :: (Show ann) => BCExprMainF ann -> Staged.ExprMainF ann
stageExpr0Main = \case
  BLiteral lit ->
    Staged.Literal (convertLiteral stageExpr0 lit)
  BConstructor (ms, ctor) ->
    Staged.Constructor (ms, ctor)
  BVar (ms, x) ->
    Staged.Var (ms, x)
  BLam Nothing labelOpt (x, tye1) e2 ->
    Staged.Lam Nothing labelOpt (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  BLam (Just (f, tyeRec)) labelOpt (x, tye1) e2 ->
    Staged.Lam (Just (f, stageTypeExpr0 tyeRec)) labelOpt (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  BApp e1 labelOpt e2 ->
    Staged.App (stageExpr0 e1) labelOpt (stageExpr0 e2)
  BLetIn x e1 e2 ->
    Staged.LetIn x [] Nothing (stageExpr0 e1) (stageExpr0 e2)
  BLetTupleIn xs e1 e2 ->
    Staged.LetTupleIn xs (stageExpr0 e1) (stageExpr0 e2)
  BLetOpenIn m e ->
    Staged.LetOpenIn m (stageExpr0 e)
  BSequential e1 e2 ->
    Staged.Sequential (stageExpr0 e1) (stageExpr0 e2)
  BTuple es ->
    Staged.Tuple (fmap stageExpr0 es)
  BIfThenElse e0 e1 e2 ->
    Staged.IfThenElse (stageExpr0 e0) (stageExpr0 e1) (stageExpr0 e2)
  BAs e1 tye2 ->
    Staged.As (stageExpr0 e1) (stageTypeExpr0 tye2)
  BLamInf (x, tye1) e2 ->
    Staged.LamInf (x, stageTypeExpr0 tye1) (stageExpr0 e2)
  BAppInfGiven e1 e2 ->
    Staged.AppInfGiven (stageExpr0 e1) (stageExpr0 e2)
  BAppInfOmitted e1 ->
    Staged.AppInfOmitted (stageExpr0 e1)

stageExpr1 :: (Show ann) => BCExprF ann -> Staged.ExprF ann
stageExpr1 (BExpr (btc, ann) exprMain) =
  case btc of
    BT0 -> Staged.Expr ann (Staged.Escape (Staged.Expr ann (stageExpr0Main exprMain)))
    BT1 -> Staged.Expr ann (stageExpr1Main exprMain)

stageExpr1Main :: (Show ann) => BCExprMainF ann -> Staged.ExprMainF ann
stageExpr1Main = \case
  BLiteral lit ->
    Staged.Literal (convertLiteral stageExpr1 lit)
  BConstructor (ms, ctor) ->
    Staged.Constructor (ms, ctor)
  BVar (ms, x) ->
    Staged.Var (ms, x)
  BLam Nothing labelOpt (x, tye1) e2 ->
    Staged.Lam Nothing labelOpt (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  BLam (Just (f, tyeRec)) labelOpt (x, tye1) e2 ->
    Staged.Lam (Just (f, stageTypeExpr1 tyeRec)) labelOpt (x, stageTypeExpr1 tye1) (stageExpr1 e2)
  BApp e1 labelOpt e2 ->
    Staged.App (stageExpr1 e1) labelOpt (stageExpr1 e2)
  BLetIn x e1 e2 ->
    Staged.LetIn x [] Nothing (stageExpr1 e1) (stageExpr1 e2)
  BLetTupleIn xs e1 e2 ->
    Staged.LetTupleIn xs (stageExpr1 e1) (stageExpr1 e2)
  BLetOpenIn m e ->
    Staged.LetOpenIn m (stageExpr1 e)
  BSequential e1 e2 ->
    Staged.Sequential (stageExpr1 e1) (stageExpr1 e2)
  BTuple es ->
    Staged.Tuple (fmap stageExpr1 es)
  BIfThenElse e0 e1 e2 ->
    Staged.IfThenElse (stageExpr1 e0) (stageExpr1 e1) (stageExpr1 e2)
  BAs e1 tye2 ->
    Staged.As (stageExpr1 e1) (stageTypeExpr1 tye2)
  BLamInf (_x, _tye1) _e2 ->
    error "bug: stageExpr1Main, BLamInf"
  BAppInfGiven _e1 _e2 ->
    error "bug: stageExpr1Main, BAppInfGiven"
  BAppInfOmitted _e1 ->
    error "bug: stageExpr1Main, BAppInfOmitted"

tyCode :: Staged.TypeExprF ann -> Staged.TypeExprMainF ann
tyCode = Staged.Bracket

tyNameWithArgs :: TypeName -> [Staged.ExprF ann] -> Staged.TypeExprMainF ann
tyNameWithArgs tyName =
  foldl'
    (\eMain argExpr -> Staged.App (Staged.Expr ann eMain) Nothing argExpr)
    (Staged.Constructor ([], tyName))
  where
    ann = error "TODO: tyNameWithArgs, ann"

stageTypeExpr0 :: (Show ann) => BCTypeExprF ann -> Staged.TypeExprF ann
stageTypeExpr0 (BTypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT1 -> Staged.Expr ann (tyCode (Staged.Expr ann (stageTypeExpr1Main typeExprMain)))
    BT0 -> Staged.Expr ann (stageTypeExpr0Main typeExprMain)

stageTypeExpr0Main :: (Show ann) => BCTypeExprMainF ann -> Staged.TypeExprMainF ann
stageTypeExpr0Main = \case
  BTyName tyName args ->
    -- TODO: check that `ExprArg` only contains literals
    tyNameWithArgs tyName $
      map stageArgForType0 args
  BTyArrow labelOpt (xOpt, tye1) tye2 ->
    Staged.TyArrow labelOpt (xOpt, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)
  BTyInfArrow (x, tye1) tye2 ->
    Staged.TyInfArrow (x, stageTypeExpr0 tye1) (stageTypeExpr0 tye2)
  BTyRefinement x tye1 e2 ->
    Staged.TyRefinement x (stageTypeExpr0 tye1) (stageExpr0 e2)
  BTyProduct tye1 rest ->
    Staged.Product
      (stageTypeExpr0 tye1)
      (fmap (\(locAster, tye) -> ((locAster, "*"), stageTypeExpr0 tye)) rest)

stageArgForType0 :: (Show ann) => BCArgForTypeF ann -> Staged.ExprF ann
stageArgForType0 = \case
  BExprArg e -> stageExpr0 e
  BTypeExprArg tye -> stageTypeExpr0 tye

stageTypeExpr1 :: (Show ann) => BCTypeExprF ann -> Staged.TypeExprF ann
stageTypeExpr1 (BTypeExpr (btc, ann) typeExprMain) =
  case btc of
    BT0 -> error $ "bug: stageTypeExpr1, BT0; " ++ show typeExprMain
    BT1 -> Staged.Expr ann (stageTypeExpr1Main typeExprMain)

stageTypeExpr1Main :: (Show ann) => BCTypeExprMainF ann -> Staged.TypeExprMainF ann
stageTypeExpr1Main = \case
  BTyName tyName args -> tyNameWithArgs tyName (map stageArgForType1 args)
  BTyArrow labelOpt (_xOpt, tye1) tye2 -> Staged.TyArrow labelOpt (Nothing, stageTypeExpr1 tye1) (stageTypeExpr1 tye2)
  BTyInfArrow (_x, _tye1) _tye2 -> error "bug: stageTypeExpr1Main, BTyInfArrow"
  BTyRefinement _x _tye _e -> error "bug: stageTypeExpr1Main, BTyRefinement"
  BTyProduct tye1 rest ->
    Staged.Product
      (stageTypeExpr1 tye1)
      (fmap (\(locAster, tye) -> ((locAster, "*"), stageTypeExpr1 tye)) rest)

stageArgForType1 :: (Show ann) => BCArgForTypeF ann -> Staged.ExprF ann
stageArgForType1 = \case
  BExprArg e@(BExpr (_btc, ann) _) -> Staged.Expr ann (Staged.Persistent (stageExpr0 e))
  BTypeExprArg tye -> stageTypeExpr1 tye

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
