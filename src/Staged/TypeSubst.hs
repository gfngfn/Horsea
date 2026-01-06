module Staged.TypeSubst
  ( HasTypeVar (..),
    tySubst0,
    tySubst1,
  )
where

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

instance HasTypeVar Ass1TypeExprF where
  tySubst :: forall sv. TypeSubstF sv -> Ass1TypeExprF sv -> Ass1TypeExprF sv
  tySubst s = \case
    A1TyPrim a1tyPrim -> A1TyPrim a1tyPrim
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

instance HasTypeVar Ass0ExprF where
  tySubst = error "TODO: HasTypeVar Ass0ExprF"
