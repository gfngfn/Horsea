module Staged.TypeSubst
  ( HasTypeVar (..),
    tySubst0,
    tySubst1,
  )
where

import Data.Functor.Identity
import Staged.Core
import Staged.Syntax
import Util.Maybe1
import Prelude

data TypeSubstF sv
  = TypeSubst0 AssTypeVar (Ass0TypeExprF sv)
  | TypeSubst1 AssTypeVar (Ass1TypeExprF sv)

class HasTypeVar af where
  tySubst :: forall sv. TypeSubstF sv -> af sv -> af sv

tySubst0 :: (HasTypeVar af) => Ass0TypeExprF sv -> AssTypeVar -> af sv -> af sv
tySubst0 a0tye atyvar = tySubst (TypeSubst0 atyvar a0tye)

tySubst1 :: (HasTypeVar af) => Ass1TypeExprF sv -> AssTypeVar -> af sv -> af sv
tySubst1 a1tye atyvar = tySubst (TypeSubst1 atyvar a1tye)

instance (HasTypeVar af) => HasTypeVar (Maybe1 af) where
  tySubst s = Maybe1 . fmap (tySubst s) . unMaybe1

instance HasTypeVar Ass0TypeExprF where
  tySubst :: forall sv. TypeSubstF sv -> Ass0TypeExprF sv -> Ass0TypeExprF sv
  tySubst s = \case
    A0TyPrim a0tyPrim maybePred ->
      A0TyPrim a0tyPrim ((unMaybe1 . go . Maybe1) maybePred)
    A0TyVar atyvar ->
      case s of
        TypeSubst0 atyvar' a0tye' -> if atyvar == atyvar' then a0tye' else A0TyVar atyvar
        TypeSubst1 _ _ -> A0TyVar atyvar
    A0TyList a0tye1 maybePred ->
      A0TyList (go a0tye1) ((unMaybe1 . go . Maybe1) maybePred)
    A0TyProduct a0tye1 a0tye2 ->
      A0TyProduct (go a0tye1) (go a0tye2)
    A0TyArrow labelOpt (svOpt, a0tye1) a0tye2 ->
      A0TyArrow labelOpt (svOpt, go a0tye1) (go a0tye2)
    A0TyOptArrow (ax, a0tye1) a0tye2 ->
      A0TyOptArrow (ax, go a0tye1) (go a0tye2)
    A0TyCode a1tye1 ->
      A0TyCode (go a1tye1)
    A0TyImplicitForAll atyvar a0tye1 ->
      A0TyImplicitForAll atyvar $
        case s of
          TypeSubst0 atyvar' _ -> if atyvar == atyvar' then a0tye1 else go a0tye1
          TypeSubst1 _ _ -> go a0tye1
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst s

instance HasTypeVar StrictAss0TypeExprF where
  tySubst :: forall sv. TypeSubstF sv -> StrictAss0TypeExprF sv -> StrictAss0TypeExprF sv
  tySubst s = \case
    SA0TyPrim a0tyPrim maybePred ->
      SA0TyPrim a0tyPrim ((unMaybe1 . go . Maybe1) maybePred)
    SA0TyVar atyvar ->
      case s of
        TypeSubst0 atyvar' a0tye' -> if atyvar == atyvar' then strictify a0tye' else SA0TyVar atyvar
        TypeSubst1 _ _ -> SA0TyVar atyvar
    SA0TyList sa0tye1 maybePred ->
      SA0TyList (go sa0tye1) ((unMaybe1 . go . Maybe1) maybePred)
    SA0TyProduct sa0tye1 sa0tye2 ->
      SA0TyProduct (go sa0tye1) (go sa0tye2)
    SA0TyArrow (svOpt, sa0tye1) sa0tye2 ->
      SA0TyArrow (svOpt, go sa0tye1) (go sa0tye2)
    SA0TyCode a1tye1 ->
      SA0TyCode (go a1tye1)
    SA0TyExplicitForAll atyvar sa0tye1 ->
      SA0TyExplicitForAll atyvar $
        case s of
          TypeSubst0 atyvar' _ -> if atyvar == atyvar' then sa0tye1 else go sa0tye1
          TypeSubst1 _ _ -> go sa0tye1
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst s

instance HasTypeVar Ass1TypeExprF where
  tySubst :: forall sv. TypeSubstF sv -> Ass1TypeExprF sv -> Ass1TypeExprF sv
  tySubst s = \case
    A1TyPrim a1tyPrim -> A1TyPrim (go a1tyPrim)
    A1TyList a1tye1 -> A1TyList (go a1tye1)
    A1TyVar atyvar ->
      case s of
        TypeSubst0 _ _ -> A1TyVar atyvar
        TypeSubst1 atyvar' a1tye' -> if atyvar == atyvar' then a1tye' else A1TyVar atyvar
    A1TyProduct a1tye1 a1tye2 -> A1TyProduct (go a1tye1) (go a1tye2)
    A1TyArrow labelOpt a1tye1 a1tye2 -> A1TyArrow labelOpt (go a1tye1) (go a1tye2)
    A1TyImplicitForAll atyvar a1tye2 ->
      A1TyImplicitForAll atyvar $
        case s of
          TypeSubst0 _ _ -> go a1tye2
          TypeSubst1 atyvar' _ -> if atyvar == atyvar' then a1tye2 else go a1tye2
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst s

instance HasTypeVar Ass1PrimTypeF where
  tySubst :: forall sv. TypeSubstF sv -> Ass1PrimTypeF sv -> Ass1PrimTypeF sv
  tySubst s = \case
    A1TyPrimBase bty ->
      A1TyPrimBase bty
    A1TyTensor a0e ->
      A1TyTensor (go a0e)
    A1TyDataset dp ->
      A1TyDataset
        DatasetParam
          { numTrain = go dp.numTrain,
            numTest = go dp.numTest,
            image = Identity (go (runIdentity dp.image)),
            label = Identity (go (runIdentity dp.label))
          }
    A1TyLstm a0e1 a0e2 ->
      A1TyLstm (go a0e1) (go a0e2)
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst s

instance HasTypeVar Ass0ExprF where
  tySubst :: forall sv. TypeSubstF sv -> Ass0ExprF sv -> Ass0ExprF sv
  tySubst s = \case
    A0Literal alit -> A0Literal (go alit)
    A0BuiltInName bi -> A0BuiltInName bi
    A0Var x -> A0Var x
    A0Lam Nothing (x, sa0tye1) a0e2 -> A0Lam Nothing (x, go sa0tye1) (go a0e2)
    A0Lam (Just (f, sa0tye0)) (x, sa0tye1) a0e2 -> A0Lam (Just (f, go sa0tye0)) (x, go sa0tye1) (go a0e2)
    A0App a0e1 a0e2 -> A0App (go a0e1) (go a0e2)
    A0LetIn (x, sa0tye0) a0e1 a0e2 -> A0LetIn (x, go sa0tye0) (go a0e1) (go a0e2)
    A0LetTupleIn x y a0e1 a0e2 -> A0LetTupleIn x y (go a0e1) (go a0e2)
    A0Sequential a0e1 a0e2 -> A0Sequential (go a0e1) (go a0e2)
    A0Tuple a0e1 a0e2 -> A0Tuple (go a0e1) (go a0e2)
    A0IfThenElse a0e0 a0e1 a0e2 -> A0IfThenElse (go a0e0) (go a0e1) (go a0e2)
    A0Bracket a1e -> A0Bracket (go a1e)
    A0TyEqAssert _loc _ty1eq -> error "TODO: HasTypeVar Ass0ExprF, A0TyEqAssert"
    A0RefinementAssert loc a0e1 a0e2 -> A0RefinementAssert loc (go a0e1) (go a0e2)
    A0AppType a0e1 sa0tye2 -> A0AppType (go a0e1) (go sa0tye2)
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst s

instance (HasTypeVar af) => HasTypeVar (AssLiteralF af) where
  tySubst s = \case
    ALitList a0es -> ALitList (map (tySubst s) a0es)
    alit -> alit

instance HasTypeVar Ass1ExprF where
  tySubst :: forall sv. TypeSubstF sv -> Ass1ExprF sv -> Ass1ExprF sv
  tySubst s = \case
    A1Literal alit -> A1Literal (go alit)
    A1Var x -> A1Var x
    A1BuiltInName bi -> A1BuiltInName bi
    A1Lam Nothing (x, a1tye1) a1e2 -> A1Lam Nothing (x, go a1tye1) (go a1e2)
    A1Lam (Just (f, a1tye0)) (x, a1tye1) a1e2 -> A1Lam (Just (f, go a1tye0)) (x, go a1tye1) (go a1e2)
    A1App a1e1 a1e2 -> A1App (go a1e1) (go a1e2)
    A1LetTupleIn x y a1e1 a1e2 -> A1LetTupleIn x y (go a1e1) (go a1e2)
    A1Sequential a1e1 a1e2 -> A1Sequential (go a1e1) (go a1e2)
    A1Tuple a1e1 a1e2 -> A1Tuple (go a1e1) (go a1e2)
    A1IfThenElse a1e0 a1e1 a1e2 -> A1IfThenElse (go a1e0) (go a1e1) (go a1e2)
    A1Escape a0e -> A1Escape (go a0e)
    A1AppType a1e1 a1tye2 -> A1AppType (go a1e1) (go a1tye2)
    where
      go :: forall af. (HasTypeVar af) => af sv -> af sv
      go = tySubst s
