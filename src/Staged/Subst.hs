module Staged.Subst
  ( HasVar (..),
    frees0,
    frees1,
    subst0,
    subst1,
    occurs0,
    occurs1,
  )
where

import Data.Functor.Identity
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple.Extra
import Safe.Exact
import Staged.Core
import Staged.Syntax
import Util.Maybe1
import Prelude

-- TODO (refactor): use `Traversal` to implement `occurs0` and `subst0`

data SubstF sv
  = Subst0 (AssVarF sv) (Ass0ExprF sv)
  | Subst1 (AssVarF sv) (Ass1ExprF sv)

class (Ord sv) => HasVar sv af where
  frees :: af sv -> (Set (AssVarF sv), Set (AssVarF sv))
  subst :: SubstF sv -> af sv -> af sv
  alphaEquivalent :: af sv -> af sv -> Bool

frees0 :: (HasVar sv af) => af sv -> Set (AssVarF sv)
frees0 = fst . frees

frees1 :: (HasVar sv af) => af sv -> Set (AssVarF sv)
frees1 = snd . frees

subst0 :: (HasVar sv af) => Ass0ExprF sv -> AssVarF sv -> af sv -> af sv
subst0 a0e x = subst (Subst0 x a0e)

subst1 :: (HasVar sv af) => Ass1ExprF sv -> AssVarF sv -> af sv -> af sv
subst1 a1e x = subst (Subst1 x a1e)

occurs0 :: (HasVar sv af) => AssVarF sv -> af sv -> Bool
occurs0 x e = x `elem` frees0 e

occurs1 :: (HasVar sv af) => AssVarF sv -> af sv -> Bool
occurs1 x e = x `elem` frees1 e

unionPairs :: (Ord sv) => [(Set (AssVarF sv), Set (AssVarF sv))] -> (Set (AssVarF sv), Set (AssVarF sv))
unionPairs pairs =
  (Set.unions (map fst pairs), Set.unions (map snd pairs))

instance (Ord sv, HasVar sv af) => HasVar sv (AssLiteralF af) where
  frees = \case
    ALitInt _ -> (Set.empty, Set.empty)
    ALitFloat _ -> (Set.empty, Set.empty)
    ALitBool _ -> (Set.empty, Set.empty)
    ALitUnit -> (Set.empty, Set.empty)
    ALitString _ -> (Set.empty, Set.empty)
    ALitList es -> unionPairs (map frees es)
    ALitVec _ -> (Set.empty, Set.empty)
    ALitMat _ -> (Set.empty, Set.empty)

  subst s = \case
    ALitInt n -> ALitInt n
    ALitFloat r -> ALitFloat r
    ALitBool b -> ALitBool b
    ALitUnit -> ALitUnit
    ALitString t -> ALitString t
    ALitList es -> ALitList (map (subst s) es)
    ALitVec vec -> ALitVec vec
    ALitMat mat -> ALitMat mat

  alphaEquivalent alit1 alit2 =
    case (alit1, alit2) of
      (ALitInt n1, ALitInt n2) -> n1 == n2
      (ALitBool b1, ALitBool b2) -> b1 == b2
      (ALitList es1, ALitList es2) ->
        case zipExactMay es1 es2 of
          Nothing -> False
          Just zipped -> all (uncurry alphaEquivalent) zipped
      (ALitVec vec1, ALitVec vec2) -> vec1 == vec2
      (ALitMat mat1, ALitMat mat2) -> mat1 == mat2
      (_, _) -> False

instance (Ord sv) => HasVar sv Ass0ExprF where
  frees = \case
    A0Literal alit ->
      frees alit
    A0Var y ->
      (Set.singleton y, Set.empty)
    A0BuiltInName _ ->
      (Set.empty, Set.empty)
    A0Lam Nothing (y, a0tye1) a0e2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0e2
       in (Set.union var0set1 (Set.delete y var0set2), Set.union var1set1 var1set2)
    A0Lam (Just (f, a0tyeRec)) (y, a0tye1) a0e2 ->
      let (var0setRec, var1setRec) = frees a0tyeRec
          (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0e2
          var0set = Set.unions [var0setRec, var0set1, Set.delete f (Set.delete y var0set2)]
          var1set = Set.unions [var1setRec, var1set1, var1set2]
       in (var0set, var1set)
    A0App a0e1 a0e2 ->
      unionPairs [frees a0e1, frees a0e2]
    A0LetIn (y, a0tye1) a0e1 a0e2 ->
      frees (A0App (A0Lam Nothing (y, a0tye1) a0e2) a0e1)
    A0LetTupleIn xL xR a0e1 a0e2 ->
      let (var0set1, var1set1) = frees a0e1
          (var0set2, var1set2) = frees a0e2
       in (Set.union var0set1 (Set.delete xL (Set.delete xR var0set2)), Set.union var1set1 var1set2)
    A0Sequential a0e1 a0e2 ->
      unionPairs [frees a0e1, frees a0e2]
    A0Tuple a0e1 a0e2 ->
      unionPairs [frees a0e1, frees a0e2]
    A0IfThenElse a0e0 a0e1 a0e2 ->
      unionPairs [frees a0e0, frees a0e1, frees a0e2]
    A0Bracket a1e1 ->
      frees a1e1
    A0TyEqAssert _ ty0eq ->
      frees ty0eq
    A0RefinementAssert _ a0ePred a0eTarget ->
      unionPairs [frees a0ePred, frees a0eTarget]
    A0AppType a0e1 a0tye2 ->
      unionPairs [frees a0e1, frees a0tye2]

  subst s = \case
    A0Literal alit ->
      A0Literal (go alit)
    A0Var y ->
      case s of
        Subst0 x a0e -> if y == x then a0e else A0Var y
        Subst1 _ _ -> A0Var y
    A0BuiltInName builtInName ->
      A0BuiltInName builtInName
    A0Lam Nothing (y, a0tye1) a0e2 ->
      A0Lam Nothing (y, go a0tye1) $
        case s of
          Subst0 x _ -> if y == x then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0Lam (Just (f, a0tyeRec)) (y, a0tye1) a0e2 ->
      A0Lam (Just (f, go a0tyeRec)) (y, go a0tye1) $
        case s of
          Subst0 x _ -> if f == x || y == x then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0App a0e1 a0e2 ->
      A0App (go a0e1) (go a0e2)
    A0LetIn (y, a0tye1) a0e1 a0e2 ->
      A0LetIn (y, go a0tye1) (go a0e1) $
        case s of
          Subst0 x _ -> if y == x then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0LetTupleIn xL xR a0e1 a0e2 ->
      A0LetTupleIn xL xR (go a0e1) $
        case s of
          Subst0 x _ -> if xL == x || xR == x then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0Sequential a0e1 a0e2 ->
      A0Sequential (go a0e1) (go a0e2)
    A0Tuple a0e1 a0e2 ->
      A0Tuple (go a0e1) (go a0e2)
    A0IfThenElse a0e0 a0e1 a0e2 ->
      A0IfThenElse (go a0e0) (go a0e1) (go a0e2)
    A0Bracket a1e1 ->
      A0Bracket (go a1e1)
    A0TyEqAssert loc ty0eq ->
      A0TyEqAssert loc (go ty0eq)
    A0RefinementAssert loc a0ePred a0eTarget ->
      A0RefinementAssert loc (go a0ePred) (go a0eTarget)
    A0AppType a0e1 a0tye2 ->
      A0AppType (go a0e1) (go a0tye2)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent a0e1 a0e2 =
    case (a0e1, a0e2) of
      (A0Literal alit1, A0Literal alit2) ->
        go alit1 alit2
      (A0Var x1, A0Var x2) ->
        x1 == x2
      (A0BuiltInName builtInName1, A0BuiltInName builtInName2) ->
        builtInName1 == builtInName2
      (A0Lam Nothing (x1, a0tye11) a0e12, A0Lam Nothing (x2, a0tye21) a0e22) ->
        go a0tye11 a0tye21 && go a0e12 (subst0 (A0Var x1) x2 a0e22)
      (A0Lam (Just (f1, a0tye1Rec)) (x1, a0tye11) a0e12, A0Lam (Just (f2, a0tye2Rec)) (x2, a0tye21) a0e22) ->
        go a0tye1Rec a0tye2Rec
          && go a0tye11 a0tye21
          && go a0e12 (subst0 (A0Var f1) f2 (subst0 (A0Var x1) x2 a0e22))
      (A0App a0e11 a0e12, A0App a0e21 a0e22) ->
        go a0e11 a0e21 && go a0e12 a0e22
      (A0LetIn (x1, a0tye1) a0e11 a0e12, A0LetIn (x2, a0tye2) a0e21 a0e22) ->
        go a0tye1 a0tye2 && go a0e11 a0e21 && go a0e12 (subst0 (A0Var x1) x2 a0e22)
      (A0LetTupleIn x1L x1R a0e11 a0e12, A0LetTupleIn x2L x2R a0e21 a0e22) ->
        go a0e11 a0e21 && go a0e12 (subst0 (A0Var x1R) x2R (subst0 (A0Var x1L) x2L a0e22))
      (A0Sequential a0e11 a0e12, A0Sequential a0e21 a0e22) ->
        go a0e11 a0e21 && go a0e12 a0e22
      (A0Tuple a0e11 a0e12, A0Tuple a0e21 a0e22) ->
        go a0e11 a0e21 && go a0e12 a0e22
      (A0IfThenElse a0e10 a0e11 a0e12, A0IfThenElse a0e20 a0e21 a0e22) ->
        go a0e10 a0e20 && go a0e11 a0e21 && go a0e12 a0e22
      (A0Bracket a1e1, A0Bracket a1e2) ->
        go a1e1 a1e2
      (A0TyEqAssert _ ty0eq1, A0TyEqAssert _ ty0eq2) ->
        go ty0eq1 ty0eq2
      (A0RefinementAssert _ a0ePred1 a0eTarget1, A0RefinementAssert _ a0ePred2 a0eTarget2) ->
        go a0ePred1 a0ePred2 && go a0eTarget1 a0eTarget2
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv Ass1ExprF where
  frees = \case
    A1Literal alit ->
      frees alit
    A1Var y ->
      (Set.empty, Set.singleton y)
    A1BuiltInName _ ->
      (Set.empty, Set.empty)
    A1Lam Nothing (y, a1tye1) a1e2 ->
      let (var0set1, var1set1) = frees a1tye1
          (var0set2, var1set2) = frees a1e2
       in (Set.union var0set1 var0set2, Set.union var1set1 (Set.delete y var1set2))
    A1Lam (Just (f, a1tyeRec)) (y, a1tye1) a1e2 ->
      let (var0setRec, var1setRec) = frees a1tyeRec
          (var0set1, var1set1) = frees a1tye1
          (var0set2, var1set2) = frees a1e2
          var0set = Set.unions [var0setRec, var0set1, var0set2]
          var1set = Set.unions [var1setRec, var1set1, Set.delete f (Set.delete y var1set2)]
       in (var0set, var1set)
    A1App a1e1 a1e2 ->
      unionPairs [frees a1e1, frees a1e2]
    A1LetIn (x, a1tye0) a1e1 a1e2 ->
      let (var0set0, var1set0) = frees a1tye0
          (var0set1, var1set1) = frees a1e1
          (var0set2, var1set2) = frees a1e2
       in (Set.unions [var0set0, var0set1, var0set2], Set.unions [var1set0, var1set1, Set.delete x var1set2])
    A1LetTupleIn xL xR a1e1 a1e2 ->
      let (var0set1, var1set1) = frees a1e1
          (var0set2, var1set2) = frees a1e2
       in (Set.union var0set1 var0set2, Set.union var1set1 (Set.delete xL (Set.delete xR var1set2)))
    A1Sequential a1e1 a1e2 ->
      unionPairs [frees a1e1, frees a1e2]
    A1Tuple a1e1 a1e2 ->
      unionPairs [frees a1e1, frees a1e2]
    A1IfThenElse a1e0 a1e1 a1e2 ->
      unionPairs [frees a1e0, frees a1e1, frees a1e2]
    A1Escape a0e1 ->
      frees a0e1
    A1AppType a1e1 a1tye2 ->
      unionPairs [frees a1e1, frees a1tye2]

  subst s = \case
    A1Literal alit ->
      A1Literal (go alit)
    A1Var y ->
      case s of
        Subst0 _ _ -> A1Var y
        Subst1 x a1e -> if y == x then a1e else A1Var y
    A1BuiltInName a1builtInName ->
      A1BuiltInName a1builtInName
    A1Lam Nothing (y, a1tye1) a1e2 ->
      A1Lam Nothing (y, go a1tye1) $
        case s of
          Subst0 _ _ -> go a1e2
          Subst1 x _ -> if y == x then a1e2 else go a1e2
    A1Lam (Just (f, a1tyeRec)) (y, a1tye1) a1e2 ->
      A1Lam (Just (f, go a1tyeRec)) (y, go a1tye1) $
        case s of
          Subst0 x _ -> if y == f || y == x then a1e2 else go a1e2
          Subst1 _ _ -> go a1e2
    A1App a1e1 a1e2 ->
      A1App (go a1e1) (go a1e2)
    A1LetIn (x', a1tye0) a1e1 a1e2 ->
      A1LetIn (x', go a1tye0) (go a1e1) $
        case s of
          Subst0 x _ -> if x == x' then a1e2 else go a1e2
          Subst1 _ _ -> go a1e2
    A1LetTupleIn xL xR a1e1 a1e2 ->
      A1LetTupleIn xL xR (go a1e1) $
        case s of
          Subst0 x _ -> if x == xL || x == xR then a1e2 else go a1e2
          Subst1 _ _ -> go a1e2
    A1Sequential a1e1 a1e2 ->
      A1Sequential (go a1e1) (go a1e2)
    A1Tuple a1e1 a1e2 ->
      A1Tuple (go a1e1) (go a1e2)
    A1IfThenElse a1e0 a1e1 a1e2 ->
      A1IfThenElse (go a1e0) (go a1e1) (go a1e2)
    A1Escape a0e1 ->
      A1Escape (go a0e1)
    A1AppType a1e1 a1tye2 ->
      A1AppType (go a1e1) (go a1tye2)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent a1e1 a1e2 =
    case (a1e1, a1e2) of
      (A1Literal lit1, A1Literal lit2) ->
        go lit1 lit2
      (A1Var x1, A1Var x2) ->
        x1 == x2
      (A1Lam Nothing (x1, a1tye11) a1e12, A1Lam Nothing (x2, a1tye21) a1e22) ->
        go a1tye11 a1tye21
          && go a1e12 (subst1 (A1Var x1) x2 a1e22)
      (A1Lam (Just (f1, a1tye1Rec)) (x1, a1tye11) a1e12, A1Lam (Just (f2, a1tye2Rec)) (x2, a1tye21) a1e22) ->
        go a1tye1Rec a1tye2Rec
          && go a1tye11 a1tye21
          && go a1e12 (subst1 (A1Var x1) x2 (subst1 (A1Var f1) f2 a1e22))
      (A1App a1e11 a1e12, A1App a1e21 a1e22) ->
        go a1e11 a1e21 && go a1e12 a1e22
      (A1LetTupleIn x1L x1R a1e11 a1e12, A1LetTupleIn x2L x2R a1e21 a1e22) ->
        go a1e11 a1e21 && go a1e12 (subst0 (A0Var x1R) x2R (subst0 (A0Var x1L) x2L a1e22))
      (A1Sequential a1e11 a1e12, A1Sequential a1e21 a1e22) ->
        go a1e11 a1e21 && go a1e12 a1e22
      (A1Tuple a1e11 a1e12, A1Tuple a1e21 a1e22) ->
        go a1e11 a1e21 && go a1e12 a1e22
      (A1IfThenElse a1e10 a1e11 a1e12, A1IfThenElse a1e20 a1e21 a1e22) ->
        go a1e10 a1e20 && go a1e11 a1e21 && go a1e12 a1e22
      (A1Escape a0e1, A1Escape a0e2) ->
        go a0e1 a0e2
      (A1AppType a1e11 a1tye12, A1AppType a1e21 a1tye22) ->
        go a1e11 a1e21 && go a1tye12 a1tye22
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv Ass0TypeExprF where
  frees = \case
    A0TyPrim _ maybePred ->
      frees (Maybe1 maybePred)
    A0TyVar _atyvar ->
      (Set.empty, Set.empty)
    A0TyList a0tye maybePred ->
      unionPairs [frees a0tye, frees (Maybe1 maybePred)]
    A0TyProduct a0tye1 a0tye2 ->
      unionPairs [frees a0tye1, frees a0tye2]
    A0TyArrow _labelOpt (yOpt, a0tye1) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0tye2
          var0set =
            Set.union var0set1 $
              case yOpt of
                Nothing -> var0set2
                Just y -> Set.delete y var0set2
          var1set = Set.union var1set1 var1set2
       in (var0set, var1set)
    A0TyCode a1tye1 ->
      frees a1tye1
    A0TyInfArrow (y, a0tye1) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0tye2
          var0set = Set.union var0set1 (Set.delete y var0set2)
          var1set = Set.union var1set1 var1set2
       in (var0set, var1set)
    A0TyOmsArrow (yOpt, a0tye1, a0e0) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set0, var1set0) = frees a0e0
          (var0set2, var1set2) = frees a0tye2
          var0set =
            Set.union (Set.union var0set1 var0set0) $
              case yOpt of
                Just y -> Set.delete y var0set2
                Nothing -> var0set2
          var1set = Set.unions [var1set1, var1set0, var1set2]
       in (var0set, var1set)
    A0TyImplicitForAll _atyvar a0tye ->
      frees a0tye

  subst s = \case
    A0TyPrim a0tyPrim maybePred ->
      A0TyPrim a0tyPrim (unMaybe1 . go . Maybe1 $ maybePred)
    A0TyVar atyvar ->
      A0TyVar atyvar
    A0TyList a0tye maybePred ->
      A0TyList (go a0tye) (unMaybe1 . go . Maybe1 $ maybePred)
    A0TyProduct a0tye1 a0tye2 ->
      A0TyProduct (go a0tye1) (go a0tye2)
    A0TyArrow labelOpt (yOpt, a0tye1) a0tye2 ->
      A0TyArrow labelOpt (yOpt, go a0tye1) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then a0tye2 else go a0tye2
          (Nothing, _) -> go a0tye2
          (_, Subst1 _ _) -> go a0tye2
    A0TyCode a1tye1 ->
      A0TyCode (go a1tye1)
    A0TyInfArrow (y, a0tye1) a0tye2 ->
      A0TyInfArrow (y, go a0tye1) $
        case s of
          Subst0 x _ -> if y == x then a0tye2 else go a0tye2
          Subst1 _ _ -> go a0tye2
    A0TyOmsArrow (yOpt, a0tye1, a0e0) a0tye2 ->
      A0TyOmsArrow (yOpt, go a0tye1, go a0e0) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then a0tye2 else go a0tye2
          (Nothing, _) -> go a0tye2
          (_, Subst1 _ _) -> go a0tye2
    A0TyImplicitForAll atyvar a0tye ->
      A0TyImplicitForAll atyvar (go a0tye)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent a0tye1 a0tye2 =
    case (a0tye1, a0tye2) of
      (A0TyPrim a0tyPrim1 maybePred1, A0TyPrim a0tyPrim2 maybePred2) ->
        -- Exact match
        a0tyPrim1 == a0tyPrim2 && go (Maybe1 maybePred1) (Maybe1 maybePred2)
      (A0TyVar atyvar1, A0TyVar atyvar2) ->
        atyvar1 == atyvar2
      (A0TyList a0tye1' maybePred1, A0TyList a0tye2' maybePred2) ->
        go a0tye1' a0tye2' && go (Maybe1 maybePred1) (Maybe1 maybePred2)
      (A0TyArrow labelOpt1 (y1opt, a0tye11) a0tye12, A0TyArrow labelOpt2 (y2opt, a0tye21) a0tye22) ->
        labelOpt1 == labelOpt2
          && go a0tye11 a0tye21
          && case (y1opt, y2opt) of
            (Nothing, Nothing) ->
              go a0tye12 a0tye22
            (Just y1, Nothing) ->
              not (occurs0 y1 a0tye12) && go a0tye12 a0tye22
            (Nothing, Just y2) ->
              not (occurs0 y2 a0tye22) && go a0tye12 a0tye22
            (Just y1, Just y2) ->
              go a0tye12 (subst0 (A0Var y1) y2 a0tye22)
      (A0TyCode a1tye1, A0TyCode a1tye2) ->
        go a1tye1 a1tye2
      (A0TyInfArrow (y1, a0tye11) a0tye12, A0TyInfArrow (y2, a0tye21) a0tye22) ->
        go a0tye11 a0tye21 && go a0tye12 (subst0 (A0Var y1) y2 a0tye22)
      (A0TyOmsArrow (y1opt, a0tye11, a0e10) a0tye12, A0TyOmsArrow (y2opt, a0tye21, a0e20) a0tye22) ->
        go a0tye11 a0tye21
          && go a0e10 a0e20
          && case (y1opt, y2opt) of
            (Nothing, Nothing) ->
              go a0tye12 a0tye22
            (Just y1, Nothing) ->
              not (occurs0 y1 a0tye12) && go a0tye12 a0tye22
            (Nothing, Just y2) ->
              not (occurs0 y2 a0tye22) && go a0tye12 a0tye22
            (Just y1, Just y2) ->
              go a0tye12 (subst0 (A0Var y1) y2 a0tye22)
      (A0TyImplicitForAll atyvar1 a0tye1', A0TyImplicitForAll atyvar2 a0tye2') ->
        -- TODO: true alpha-equivalence
        atyvar1 == atyvar2 && go a0tye1' a0tye2'
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv Ass0TypeExprNegF where
  frees = error "TODO: frees"
  subst = error "TODO: subst"
  alphaEquivalent = error "TODO: alphaEquivalent"

instance (Ord sv) => HasVar sv Ass1TypeExprF where
  frees = \case
    A1TyPrim a1tyPrim ->
      case a1tyPrim of
        A1TyPrimBase _ ->
          (Set.empty, Set.empty)
        A1TyTensor a0eList ->
          frees a0eList
        A1TyDataset DatasetParam {numTrain, numTest, image, label} ->
          unionPairs
            [ frees numTrain,
              frees numTest,
              frees (runIdentity image),
              frees (runIdentity label)
            ]
        A1TyLstm a0e1 a0e2 ->
          unionPairs [frees a0e1, frees a0e2]
        A1TyTextHelper a0e ->
          frees a0e
    A1TyList a1tye1 ->
      frees a1tye1
    A1TyVar _atyvar ->
      (Set.empty, Set.empty)
    A1TyProduct a1tye1 a1tye2 ->
      unionPairs [frees a1tye1, frees a1tye2]
    A1TyArrow _labelOpt a1tye1 a1tye2 ->
      unionPairs [frees a1tye1, frees a1tye2]
    A1TyImplicitForAll _atyvar a1tye2 ->
      frees a1tye2

  subst s = \case
    A1TyPrim a1tyPrim ->
      A1TyPrim $ case a1tyPrim of
        A1TyPrimBase tyPrimBase -> A1TyPrimBase tyPrimBase
        A1TyTensor a0eList -> A1TyTensor (go a0eList)
        A1TyDataset datasetParam -> A1TyDataset (fmap go datasetParam)
        A1TyLstm a0e1 a0e2 -> A1TyLstm (go a0e1) (go a0e2)
        A1TyTextHelper a0e -> A1TyTextHelper (go a0e)
    A1TyList a1tye1 ->
      A1TyList (go a1tye1)
    A1TyVar atyvar ->
      A1TyVar atyvar
    A1TyProduct a1tye1 a1tye2 ->
      A1TyProduct (go a1tye1) (go a1tye2)
    A1TyArrow labelOpt a1tye1 a1tye2 ->
      A1TyArrow labelOpt (go a1tye1) (go a1tye2)
    A1TyImplicitForAll atyvar a1tye2 ->
      A1TyImplicitForAll atyvar (go a1tye2)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent a1tye1 a1tye2 =
    case (a1tye1, a1tye2) of
      (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
        case (a1tyPrim1, a1tyPrim2) of
          (A1TyPrimBase tyPrimBase1, A1TyPrimBase tyPrimBase2) ->
            tyPrimBase1 == tyPrimBase2
          (A1TyTensor a0eList1, A1TyTensor a0eList2) ->
            go a0eList1 a0eList2
          (_, _) ->
            False
      (A1TyList a1tye1', A1TyList a1tye2') ->
        go a1tye1' a1tye2'
      (A1TyArrow labelOpt1 a1tye11 a1tye12, A1TyArrow labelOpt2 a1tye21 a1tye22) ->
        labelOpt1 == labelOpt2 && go a1tye11 a1tye21 && go a1tye12 a1tye22
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv StrictAss0TypeExprF where
  frees = \case
    SA0TyPrim _ maybePred ->
      frees (Maybe1 maybePred)
    SA0TyVar _atyvar ->
      (Set.empty, Set.empty)
    SA0TyList a0tye maybePred ->
      unionPairs [frees a0tye, frees (Maybe1 maybePred)]
    SA0TyProduct a0tye1 a0tye2 ->
      unionPairs [frees a0tye1, frees a0tye2]
    SA0TyArrow (yOpt, a0tye1) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0tye2
          var0set =
            Set.union var0set1 $
              case yOpt of
                Nothing -> var0set2
                Just y -> Set.delete y var0set2
          var1set = Set.union var1set1 var1set2
       in (var0set, var1set)
    SA0TyCode a1tye1 ->
      frees a1tye1
    SA0TyExplicitForAll _atyvar a0tye ->
      frees a0tye

  subst s = \case
    SA0TyPrim a0tyPrim maybePred ->
      SA0TyPrim a0tyPrim (unMaybe1 . go . Maybe1 $ maybePred)
    SA0TyVar atyvar ->
      SA0TyVar atyvar
    SA0TyList a0tye maybePred ->
      SA0TyList (go a0tye) (unMaybe1 . go . Maybe1 $ maybePred)
    SA0TyProduct a0tye1 a0tye2 ->
      SA0TyProduct (go a0tye1) (go a0tye2)
    SA0TyArrow (yOpt, sa0tye1) sa0tye2 ->
      SA0TyArrow (yOpt, go sa0tye1) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then sa0tye2 else go sa0tye2
          (Nothing, _) -> go sa0tye2
          (_, Subst1 _ _) -> go sa0tye2
    SA0TyCode a1tye1 ->
      SA0TyCode (go a1tye1)
    SA0TyExplicitForAll atyvar a0tye ->
      SA0TyExplicitForAll atyvar (go a0tye)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent sa0tye1 sa0tye2 =
    case (sa0tye1, sa0tye2) of
      (SA0TyPrim a0tyPrim1 maybePred1, SA0TyPrim a0tyPrim2 maybePred2) ->
        -- Exact match
        a0tyPrim1 == a0tyPrim2 && go (Maybe1 maybePred1) (Maybe1 maybePred2)
      (SA0TyVar atyvar1, SA0TyVar atyvar2) ->
        atyvar1 == atyvar2
      (SA0TyArrow (y1opt, sa0tye11) sa0tye12, SA0TyArrow (y2opt, sa0tye21) sa0tye22) ->
        (go sa0tye11 sa0tye21 &&) $
          case (y1opt, y2opt) of
            (Nothing, Nothing) ->
              go sa0tye12 sa0tye22
            (Just y1, Nothing) ->
              not (occurs0 y1 sa0tye12) && go sa0tye12 sa0tye22
            (Nothing, Just y2) ->
              not (occurs0 y2 sa0tye22) && go sa0tye12 sa0tye22
            (Just y1, Just y2) ->
              go sa0tye12 (subst0 (A0Var y1) y2 sa0tye22)
      (SA0TyCode a1tye1, SA0TyCode a1tye2) ->
        go a1tye1 a1tye2
      (SA0TyExplicitForAll atyvar1 sa0tye1', SA0TyExplicitForAll atyvar2 sa0tye2') ->
        -- TODO: true alpha-equivalence
        atyvar1 == atyvar2 && go sa0tye1' sa0tye2'
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv Type1EquationF where
  frees = \case
    TyEq1Prim ty1eqPrim ->
      case ty1eqPrim of
        TyEq1PrimBase _ -> (Set.empty, Set.empty)
        TyEq1Tensor listEq -> frees listEq
        TyEq1Dataset datasetParamEq -> frees datasetParamEq
        TyEq1Lstm (i1, i2) (h1, h2) -> unionPairs [frees i1, frees i2, frees h1, frees h2]
        TyEq1TextHelper (labels1, labels2) -> unionPairs [frees labels1, frees labels2]
    TyEq1List ty1eqElem ->
      frees ty1eqElem
    TyEq1Arrow _labelOpt ty1eqDom ty1eqCod ->
      unionPairs [frees ty1eqDom, frees ty1eqCod]
    TyEq1Product ty1eq1 ty1eq2 ->
      unionPairs [frees ty1eq1, frees ty1eq2]
    TyEq1TypeVar _atyvar ->
      (Set.empty, Set.empty)
    TyEq1ImplicitForAll _atyvar ty1eq ->
      frees ty1eq

  subst s = \case
    TyEq1Prim ty1eqPrim ->
      TyEq1Prim $
        case ty1eqPrim of
          TyEq1PrimBase tyPrimBase -> TyEq1PrimBase tyPrimBase
          TyEq1Tensor listEq -> TyEq1Tensor (go listEq)
          TyEq1Dataset dpEq -> TyEq1Dataset (go dpEq)
          TyEq1Lstm (i1, i2) (h1, h2) -> TyEq1Lstm (go i1, go i2) (go h1, go h2)
          TyEq1TextHelper (labels1, labels2) -> TyEq1TextHelper (go labels1, go labels2)
    TyEq1List ty1eqElem ->
      TyEq1List (go ty1eqElem)
    TyEq1Arrow labelOpt ty1eqDom ty1eqCod ->
      TyEq1Arrow labelOpt (go ty1eqDom) (go ty1eqCod)
    TyEq1Product ty1eq1 ty1eq2 ->
      TyEq1Product (go ty1eq1) (go ty1eq2)
    TyEq1TypeVar atyvar ->
      TyEq1TypeVar atyvar
    TyEq1ImplicitForAll atyvar ty1eq ->
      TyEq1ImplicitForAll atyvar (go ty1eq)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent ty1eq1 ty1eq2 =
    case (ty1eq1, ty1eq2) of
      (TyEq1Prim ty1eqPrim1, TyEq1Prim ty1eqPrim2) ->
        case (ty1eqPrim1, ty1eqPrim2) of
          (TyEq1PrimBase tyPrimBase1, TyEq1PrimBase tyPrimBase2) -> tyPrimBase1 == tyPrimBase2
          (TyEq1Tensor listEq1, TyEq1Tensor listEq2) -> go listEq1 listEq2
          (TyEq1Dataset dpEq1, TyEq1Dataset dpEq2) -> go dpEq1 dpEq2
          (_, _) -> False
      (TyEq1List ty1eqElem1, TyEq1List ty1eqElem2) ->
        go ty1eqElem1 ty1eqElem2
      (TyEq1Arrow labelOpt1 ty1eqDom1 ty1eqCod1, TyEq1Arrow labelOpt2 ty1eqDom2 ty1eqCod2) ->
        labelOpt1 == labelOpt2 && go ty1eqDom1 ty1eqDom2 && go ty1eqCod1 ty1eqCod2
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv ListEquationF where
  frees = \case
    ListEqByElements zipped ->
      unionPairs $
        concatMap (\(a0e1, a0e2) -> [frees a0e1, frees a0e2]) zipped
    ListEqByWhole a0eList1 a0eList2 ->
      unionPairs [frees a0eList1, frees a0eList2]

  subst s = \case
    ListEqByElements zipped ->
      ListEqByElements (map (both go) zipped)
    ListEqByWhole a0eList1 a0eList2 ->
      ListEqByWhole (go a0eList1) (go a0eList2)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent listEq1 listEq2 =
    case (listEq1, listEq2) of
      (ListEqByElements zipped1, ListEqByElements zipped2) ->
        case zipExactMay zipped1 zipped2 of
          Nothing ->
            False
          Just zippedZipped ->
            all
              ( \((a0e11, a0e12), (a0e21, a0e22)) ->
                  go a0e11 a0e21 && go a0e12 a0e22
              )
              zippedZipped
      (ListEqByWhole a0eList11 a0eList12, ListEqByWhole a0eList21 a0eList22) ->
        go a0eList11 a0eList21 && go a0eList12 a0eList22
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv DatasetParamEquationF where
  frees DatasetParamEquation {numTrainEq, numTestEq, imageEq, labelEq} =
    unionPairs
      [ frees numTrain1,
        frees numTrain2,
        frees numTest1,
        frees numTest2,
        frees imageEq,
        frees labelEq
      ]
    where
      (numTrain1, numTrain2) = numTrainEq
      (numTest1, numTest2) = numTestEq

  subst s DatasetParamEquation {numTrainEq, numTestEq, imageEq, labelEq} =
    DatasetParamEquation
      { numTrainEq = both go numTrainEq,
        numTestEq = both go numTestEq,
        imageEq = go imageEq,
        labelEq = go labelEq
      }
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent dpEq1 dpEq2 =
    go numTrain11 numTrain21
      && go numTrain12 numTrain22
      && go numTest11 numTest21
      && go numTest12 numTest22
      && go dpEq1.imageEq dpEq2.imageEq
      && go dpEq1.labelEq dpEq2.labelEq
    where
      (numTrain11, numTrain12) = dpEq1.numTrainEq
      (numTrain21, numTrain22) = dpEq2.numTrainEq
      (numTest11, numTest12) = dpEq1.numTestEq
      (numTest21, numTest22) = dpEq2.numTestEq

      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (HasVar sv af) => HasVar sv (Maybe1 af) where
  frees = maybe (Set.empty, Set.empty) frees . unMaybe1

  subst s = Maybe1 . fmap (subst s) . unMaybe1

  alphaEquivalent (Maybe1 Nothing) (Maybe1 Nothing) = True
  alphaEquivalent (Maybe1 (Just v1)) (Maybe1 (Just v2)) = alphaEquivalent v1 v2
  alphaEquivalent _ _ = False

instance (HasVar sv af) => HasVar sv (ResultF af) where
  frees = \case
    Pure v -> frees v
    Cast0 cast a0tye r -> unionPairs [frees (Maybe1 cast), frees a0tye, frees r]
    Cast1 eq a1tye r -> unionPairs [frees (Maybe1 eq), frees a1tye, frees r]
    CastGiven0 cast a0tye r -> unionPairs [frees (Maybe1 cast), frees a0tye, frees r]
    FillInferred0 a0e r -> unionPairs [frees a0e, frees r]
    InsertInferred0 a0e r -> unionPairs [frees a0e, frees r]
    InsertInferredType0 a0tye r -> unionPairs [frees a0tye, frees r]
    InsertType1 a1tye r -> unionPairs [frees a1tye, frees r]

  subst s = \case
    Pure v -> Pure (go v)
    Cast0 cast a0tye r -> Cast0 (unMaybe1 . go . Maybe1 $ cast) (go a0tye) (go r)
    Cast1 eq a1tye r -> Cast1 (unMaybe1 . go . Maybe1 $ eq) (go a1tye) (go r)
    CastGiven0 cast a0tye r -> CastGiven0 (unMaybe1 . go . Maybe1 $ cast) (go a0tye) (go r)
    FillInferred0 a0e r -> FillInferred0 (go a0e) (go r)
    InsertInferred0 a0e r -> InsertInferred0 (go a0e) (go r)
    InsertInferredType0 a0tye r -> InsertInferredType0 (go a0tye) (go r)
    InsertType1 a1tye r -> InsertType1 (go a1tye) (go r)
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv
      go = subst s

  alphaEquivalent =
    error "TODO (enhance): Result a, alphaEquivalent"
