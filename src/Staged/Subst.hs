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

import Data.List.NonEmpty qualified as NonEmpty
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Map qualified as Map
import Data.Maybe1
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Tuple.Extra
import Safe.Exact (zipExactMay)
import Staged.Syntax
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
    A0LetTupleIn xs a0e1 a0e2 ->
      let (var0set1, var1set1) = frees a0e1
          (var0set2, var1set2) = frees a0e2
       in (Set.union var0set1 (foldl (flip Set.delete) var0set2 xs), Set.union var1set1 var1set2)
    A0Sequential a0e1 a0e2 ->
      unionPairs [frees a0e1, frees a0e2]
    A0Tuple a0es ->
      unionPairs (map frees (TwoOrMore.toList a0es))
    A0Record a0re ->
      unionPairs (map (frees . snd) (Map.toList a0re))
    A0FieldProj a0e _label ->
      frees a0e
    A0Constructor _ctor a0es ->
      unionPairs (map frees a0es)
    A0IfThenElse a0e0 a0e1 a0e2 ->
      unionPairs [frees a0e0, frees a0e1, frees a0e2]
    A0Case a0e0 a0branches ->
      unionPairs (frees a0e0 : map frees (NonEmpty.toList a0branches))
    A0Bracket a1e1 ->
      frees a1e1
    A0TyEqAssert _ ty0eq ->
      frees ty0eq
    A0RefinementAssert _ a0ePred a0eTarget ->
      unionPairs [frees a0ePred, frees a0eTarget]
    A0LamType _atyvar1 a0e2 ->
      frees a0e2
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
    A0LetTupleIn xs a0e1 a0e2 ->
      A0LetTupleIn xs (go a0e1) $
        case s of
          Subst0 x _ -> if x `elem` xs then a0e2 else go a0e2
          Subst1 _ _ -> go a0e2
    A0Sequential a0e1 a0e2 ->
      A0Sequential (go a0e1) (go a0e2)
    A0Tuple a0es ->
      A0Tuple (fmap go a0es)
    A0Record a0re ->
      A0Record (fmap go a0re)
    A0FieldProj a0e label ->
      A0FieldProj (go a0e) label
    A0Constructor ctor a0es ->
      A0Constructor ctor (map go a0es)
    A0IfThenElse a0e0 a0e1 a0e2 ->
      A0IfThenElse (go a0e0) (go a0e1) (go a0e2)
    A0Case a0e0 a0branches ->
      A0Case (go a0e0) (fmap go a0branches)
    A0Bracket a1e1 ->
      A0Bracket (go a1e1)
    A0TyEqAssert loc ty0eq ->
      A0TyEqAssert loc (go ty0eq)
    A0RefinementAssert loc a0ePred a0eTarget ->
      A0RefinementAssert loc (go a0ePred) (go a0eTarget)
    A0LamType atyvar1 a0e2 ->
      A0LamType atyvar1 (go a0e2)
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
      (A0LetTupleIn xs1 a0e11 a0e12, A0LetTupleIn xs2 a0e21 a0e22) ->
        case zipExactMay (TwoOrMore.toList xs1) (TwoOrMore.toList xs2) of
          Just zipped ->
            let a0e22' = foldl (\a0e (x1, x2) -> subst0 (A0Var x1) x2 a0e) a0e22 zipped
             in go a0e11 a0e21 && go a0e12 a0e22'
          Nothing ->
            False
      (A0Sequential a0e11 a0e12, A0Sequential a0e21 a0e22) ->
        go a0e11 a0e21 && go a0e12 a0e22
      (A0Tuple a0es1, A0Tuple a0es2) ->
        case zipExactMay (TwoOrMore.toList a0es1) (TwoOrMore.toList a0es2) of
          Just zipped -> all (uncurry go) zipped
          Nothing -> False
      (A0Record a0re1, A0Record a0re2) ->
        Map.keysSet a0re1 == Map.keysSet a0re2
          && all (uncurry go . snd) (Map.toList (Map.intersectionWith (,) a0re1 a0re2))
      (A0Constructor ctor1 a0es1, A0Constructor ctor2 a0es2) ->
        ctor1 == ctor2
          && case zipExactMay a0es1 a0es2 of
            Just zipped -> all (uncurry go) zipped
            Nothing -> False
      (A0IfThenElse a0e10 a0e11 a0e12, A0IfThenElse a0e20 a0e21 a0e22) ->
        go a0e10 a0e20 && go a0e11 a0e21 && go a0e12 a0e22
      (A0Case a0e10 a0branches1, A0Case a0e20 a0branches2) ->
        go a0e10 a0e20
          && case zipExactMay (NonEmpty.toList a0branches1) (NonEmpty.toList a0branches2) of
            Just zipped -> all (uncurry go) zipped
            Nothing -> False
      (A0Bracket a1e1, A0Bracket a1e2) ->
        go a1e1 a1e2
      (A0TyEqAssert _ ty0eq1, A0TyEqAssert _ ty0eq2) ->
        go ty0eq1 ty0eq2
      (A0RefinementAssert _ a0ePred1 a0eTarget1, A0RefinementAssert _ a0ePred2 a0eTarget2) ->
        go a0ePred1 a0ePred2 && go a0eTarget1 a0eTarget2
      (A0LamType {}, A0LamType {}) ->
        False -- TODO (enhance): `alphaEquivalent` for `A0LamType`
      (A0AppType a0e11 a0tye12, A0AppType a0e21 a0tye22) ->
        go a0e11 a0e21 && go a0tye12 a0tye22
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv Ass0BranchF where
  frees (A0Branch a0pat a0e) =
    let var0setBound = freesInPattern0 a0pat
        (var0set, var1set) = frees a0e
     in (var0set \\ var0setBound, var1set)

  subst s (A0Branch a0pat a0e) =
    A0Branch a0pat $
      case s of
        Subst0 x _ -> if x `elem` freesInPattern0 a0pat then a0e else go a0e
        Subst1 _ _ -> go a0e
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent _a0branch1 _a0branch2 =
    False --  TODO (enhance): Ass0Branch, alphaEquivalent

freesInPattern0 :: (Ord sv) => Ass0PatternF sv -> Set (AssVarF sv)
freesInPattern0 = \case
  A0PatConstructor _ctor a0pats -> Set.unions (map freesInPattern0 a0pats)
  A0PatVar x -> Set.singleton x
  A0PatBool _ -> Set.empty
  A0PatListNil -> Set.empty
  A0PatListCons a0pat1 a0pat2 -> Set.union (freesInPattern0 a0pat1) (freesInPattern0 a0pat2)

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
    A1LetTupleIn xs a1e1 a1e2 ->
      let (var0set1, var1set1) = frees a1e1
          (var0set2, var1set2) = frees a1e2
       in (Set.union var0set1 var0set2, Set.union var1set1 (foldl (flip Set.delete) var1set2 xs))
    A1Sequential a1e1 a1e2 ->
      unionPairs [frees a1e1, frees a1e2]
    A1Tuple a1es ->
      unionPairs (map frees (TwoOrMore.toList a1es))
    A1Record a1rty ->
      unionPairs (map (frees . snd) (Map.toList a1rty))
    A1FieldProj a1e _label ->
      frees a1e
    A1Constructor _ctor a1es ->
      unionPairs (map frees a1es)
    A1IfThenElse a1e0 a1e1 a1e2 ->
      unionPairs [frees a1e0, frees a1e1, frees a1e2]
    A1Case a1e0 a1branches ->
      unionPairs (frees a1e0 : fmap frees (NonEmpty.toList a1branches))
    A1Escape a0e1 ->
      frees a0e1
    A1LamType _atyvar1 a1e2 ->
      frees a1e2
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
    A1LetTupleIn xs a1e1 a1e2 ->
      A1LetTupleIn xs (go a1e1) $
        case s of
          Subst0 x _ -> if x `elem` xs then a1e2 else go a1e2
          Subst1 _ _ -> go a1e2
    A1Sequential a1e1 a1e2 ->
      A1Sequential (go a1e1) (go a1e2)
    A1Tuple a1es ->
      A1Tuple (fmap go a1es)
    A1Record a1rty ->
      A1Record (fmap go a1rty)
    A1FieldProj a1e label ->
      A1FieldProj (go a1e) label
    A1Constructor ctor a1es ->
      A1Constructor ctor (map go a1es)
    A1IfThenElse a1e0 a1e1 a1e2 ->
      A1IfThenElse (go a1e0) (go a1e1) (go a1e2)
    A1Case a1e0 a1branches ->
      A1Case (go a1e0) (fmap go a1branches)
    A1Escape a0e1 ->
      A1Escape (go a0e1)
    A1LamType atyvar1 a1e2 ->
      A1LamType atyvar1 (go a1e2)
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
      (A1LetTupleIn xs1 a1e11 a1e12, A1LetTupleIn xs2 a1e21 a1e22) ->
        case zipExactMay (TwoOrMore.toList xs1) (TwoOrMore.toList xs2) of
          Just zipped ->
            let a1e22' = foldl' (\a1e (x1, x2) -> subst0 (A0Var x1) x2 a1e) a1e22 zipped
             in go a1e11 a1e21 && go a1e12 a1e22'
          Nothing ->
            False
      (A1Sequential a1e11 a1e12, A1Sequential a1e21 a1e22) ->
        go a1e11 a1e21 && go a1e12 a1e22
      (A1Tuple a1es1, A1Tuple a1es2) ->
        case zipExactMay (TwoOrMore.toList a1es1) (TwoOrMore.toList a1es2) of
          Just zipped -> all (uncurry go) zipped
          Nothing -> False
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

instance (Ord sv) => HasVar sv Ass1BranchF where
  frees (A1Branch a1pat a1e) =
    let var1setBound = freesInPattern1 a1pat
        (var0set, var1set) = frees a1e
     in (var0set, var1set \\ var1setBound)

  subst s (A1Branch a1pat a1e) =
    A1Branch a1pat $
      case s of
        Subst0 _ _ -> go a1e
        Subst1 x _ -> if x `elem` freesInPattern1 a1pat then a1e else go a1e
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent _a0branch1 _a0branch2 =
    False -- TODO (enhance): Ass1Branch, alphaEquivalent

freesInPattern1 :: (Ord sv) => Ass1PatternF sv -> Set (AssVarF sv)
freesInPattern1 = \case
  A1PatConstructor _ctor a1pats -> Set.unions (map freesInPattern1 a1pats)
  A1PatVar x -> Set.singleton x
  A1PatBool _ -> Set.empty
  A1PatListNil -> Set.empty
  A1PatListCons a1pat1 a1pat2 -> Set.union (freesInPattern1 a1pat1) (freesInPattern1 a1pat2)

instance (Ord sv) => HasVar sv Ass0TypeExprF where
  frees = \case
    A0TyPrim _ maybePred ->
      frees (Maybe1 maybePred)
    A0TyVar _atyvar ->
      (Set.empty, Set.empty)
    A0TyList a0tye maybePred ->
      unionPairs [frees a0tye, frees (Maybe1 maybePred)]
    A0TyMaybe a0tye ->
      frees a0tye
    A0TyProduct a0tyes ->
      unionPairs (map frees (TwoOrMore.toList a0tyes))
    A0TyRecord a0rty ->
      unionPairs (map (frees . snd) (Map.toList a0rty))
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
    A0TyOmsArrow _label (yOpt, a0tye1) a0tye2 ->
      let (var0set1, var1set1) = frees a0tye1
          (var0set2, var1set2) = frees a0tye2
          var0set =
            Set.union var0set1 $
              case yOpt of
                Just y -> Set.delete y var0set2
                Nothing -> var0set2
          var1set = Set.union var1set1 var1set2
       in (var0set, var1set)
    A0TyForAll _atyvar a0tye ->
      frees a0tye

  subst s = \case
    A0TyPrim a0tyPrim maybePred ->
      A0TyPrim a0tyPrim (unMaybe1 . go . Maybe1 $ maybePred)
    A0TyVar atyvar ->
      A0TyVar atyvar
    A0TyList a0tye maybePred ->
      A0TyList (go a0tye) (unMaybe1 . go . Maybe1 $ maybePred)
    A0TyMaybe a0tye ->
      A0TyMaybe (go a0tye)
    A0TyProduct a0tyes ->
      A0TyProduct (fmap go a0tyes)
    A0TyRecord a0rty ->
      A0TyRecord (fmap go a0rty)
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
    A0TyOmsArrow label (yOpt, a0tye1) a0tye2 ->
      A0TyOmsArrow label (yOpt, go a0tye1) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then a0tye2 else go a0tye2
          (Nothing, _) -> go a0tye2
          (_, Subst1 _ _) -> go a0tye2
    A0TyForAll atyvar a0tye ->
      A0TyForAll atyvar (go a0tye)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent a0tye1 a0tye2 =
    case (a0tye1, a0tye2) of
      (A0TyPrim a0tyPrim1 maybePred1, A0TyPrim a0tyPrim2 maybePred2) ->
        -- Exact match:
        a0tyPrim1 == a0tyPrim2 && go (Maybe1 maybePred1) (Maybe1 maybePred2)
      (A0TyVar atyvar1, A0TyVar atyvar2) ->
        atyvar1 == atyvar2
      (A0TyList a0tye1' maybePred1, A0TyList a0tye2' maybePred2) ->
        go a0tye1' a0tye2' && go (Maybe1 maybePred1) (Maybe1 maybePred2)
      (A0TyMaybe a0tye1', A0TyMaybe a0tye2') ->
        go a0tye1' a0tye2'
      (A0TyProduct a0tyes1, A0TyProduct a0tyes2) ->
        case zipExactMay (TwoOrMore.toList a0tyes1) (TwoOrMore.toList a0tyes2) of
          Just zipped -> all (uncurry go) zipped
          Nothing -> False
      (A0TyRecord a0rty1, A0TyRecord a0rty2) ->
        Map.keysSet a0rty1 == Map.keysSet a0rty2
          && all (uncurry go . snd) (Map.toList (Map.intersectionWith (,) a0rty1 a0rty2))
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
      (A0TyOmsArrow label1 (y1opt, a0tye11) a0tye12, A0TyOmsArrow label2 (y2opt, a0tye21) a0tye22) ->
        label1 == label2
          && go a0tye11 a0tye21
          && case (y1opt, y2opt) of
            (Nothing, Nothing) -> go a0tye12 a0tye22
            (Just y1, Nothing) -> not (occurs0 y1 a0tye12) && go a0tye12 a0tye22
            (Nothing, Just y2) -> not (occurs0 y2 a0tye22) && go a0tye12 a0tye22
            (Just y1, Just y2) -> go a0tye12 (subst0 (A0Var y1) y2 a0tye22)
      (A0TyForAll atyvar1 a0tye1', A0TyForAll atyvar2 a0tye2') ->
        -- TODO (enhance): true alpha-equivalence
        atyvar1 == atyvar2 && go a0tye1' a0tye2'
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv Ass1TypeExprF where
  frees = \case
    A1TyPrim a1tyPrim ->
      case a1tyPrim of
        A1TyPrimBase _ ->
          (Set.empty, Set.empty)
        A1TyTensor a0eList ->
          frees a0eList
        A1TyLstm a0e1 a0e2 ->
          unionPairs [frees a0e1, frees a0e2]
        A1TyTextHelper a0e ->
          frees a0e
    A1TyList a1tye1 ->
      frees a1tye1
    A1TyMaybe a1tye1 ->
      frees a1tye1
    A1TyData _datatyId a1datatyArgs ->
      unionPairs (map frees a1datatyArgs)
    A1TyVar _atyvar ->
      (Set.empty, Set.empty)
    A1TyProduct a1tyes ->
      unionPairs (map frees (TwoOrMore.toList a1tyes))
    A1TyRecord a1rty ->
      unionPairs (map (frees . snd) (Map.toList a1rty))
    A1TyArrow _labelOpt a1tye1 a1tye2 ->
      unionPairs [frees a1tye1, frees a1tye2]
    A1TyOmsArrow _label a1tye1 a1tye2 ->
      unionPairs [frees a1tye1, frees a1tye2]
    A1TyForAll _atyvar a1tye2 ->
      frees a1tye2

  subst s = \case
    A1TyPrim a1tyPrim ->
      A1TyPrim $ case a1tyPrim of
        A1TyPrimBase tyPrimBase -> A1TyPrimBase tyPrimBase
        A1TyTensor a0eList -> A1TyTensor (go a0eList)
        A1TyLstm a0e1 a0e2 -> A1TyLstm (go a0e1) (go a0e2)
        A1TyTextHelper a0e -> A1TyTextHelper (go a0e)
    A1TyList a1tye1 ->
      A1TyList (go a1tye1)
    A1TyMaybe a1tye1 ->
      A1TyMaybe (go a1tye1)
    A1TyData datatyId a1datatyArgs ->
      A1TyData datatyId (map go a1datatyArgs)
    A1TyVar atyvar ->
      A1TyVar atyvar
    A1TyProduct a1tyes ->
      A1TyProduct (fmap go a1tyes)
    A1TyRecord a1tyes ->
      A1TyRecord (fmap go a1tyes)
    A1TyArrow labelOpt a1tye1 a1tye2 ->
      A1TyArrow labelOpt (go a1tye1) (go a1tye2)
    A1TyOmsArrow label a1tye1 a1tye2 ->
      A1TyOmsArrow label (go a1tye1) (go a1tye2)
    A1TyForAll atyvar a1tye2 ->
      A1TyForAll atyvar (go a1tye2)
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
      (A1TyMaybe a1tye1', A1TyMaybe a1tye2') ->
        go a1tye1' a1tye2'
      (A1TyData datatyId1 a1datatyArgs1, A1TyData datatyId2 a1datatyArgs2) ->
        datatyId1 == datatyId2
          && case zipExactMay a1datatyArgs1 a1datatyArgs2 of
            Just zipped -> all (uncurry go) zipped
            Nothing -> False
      (A1TyVar atyvar1, A1TyVar atyvar2) ->
        atyvar1 == atyvar2
      (A1TyProduct a1tyes1, A1TyProduct a1tyes2) ->
        case zipExactMay (TwoOrMore.toList a1tyes1) (TwoOrMore.toList a1tyes2) of
          Just zipped -> all (uncurry go) zipped
          Nothing -> False
      (A1TyRecord a1rty1, A1TyRecord a1rty2) ->
        Map.keysSet a1rty1 == Map.keysSet a1rty2
          && all (uncurry go . snd) (Map.toList (Map.intersectionWith (,) a1rty1 a1rty2))
      (A1TyArrow labelOpt1 a1tye11 a1tye12, A1TyArrow labelOpt2 a1tye21 a1tye22) ->
        labelOpt1 == labelOpt2 && go a1tye11 a1tye21 && go a1tye12 a1tye22
      (A1TyOmsArrow label1 a1tye11 a1tye12, A1TyOmsArrow label2 a1tye21 a1tye22) ->
        label1 == label2 && go a1tye11 a1tye21 && go a1tye12 a1tye22
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv Ass1DatatypeArgF where
  frees = \case
    A1DatatypeArgType a1tye -> frees a1tye
    A1DatatypeArgVal0 a0e -> frees a0e

  subst s = \case
    A1DatatypeArgType a1tye -> A1DatatypeArgType (go a1tye)
    A1DatatypeArgVal0 a0e -> A1DatatypeArgVal0 (go a0e)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent a1datatyArg1 a1datatyArg2 =
    case (a1datatyArg1, a1datatyArg2) of
      (A1DatatypeArgType a1tye1, A1DatatypeArgType a1tye2) -> go a1tye1 a1tye2
      (A1DatatypeArgVal0 a0e1, A1DatatypeArgVal0 a0e2) -> go a0e1 a0e2
      _ -> False
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
    SA0TyMaybe a0tye ->
      frees a0tye
    SA0TyProduct a0tyes ->
      unionPairs (map frees (TwoOrMore.toList a0tyes))
    SA0TyRecord a0rty ->
      unionPairs (map (frees . snd) (Map.toList a0rty))
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
    SA0TyForAll _atyvar a0tye ->
      frees a0tye

  subst s = \case
    SA0TyPrim a0tyPrim maybePred ->
      SA0TyPrim a0tyPrim (unMaybe1 . go . Maybe1 $ maybePred)
    SA0TyVar atyvar ->
      SA0TyVar atyvar
    SA0TyList a0tye maybePred ->
      SA0TyList (go a0tye) (unMaybe1 . go . Maybe1 $ maybePred)
    SA0TyMaybe a0tye ->
      SA0TyMaybe (go a0tye)
    SA0TyProduct a0tyes ->
      SA0TyProduct (fmap go a0tyes)
    SA0TyRecord a0rty ->
      SA0TyRecord (fmap go a0rty)
    SA0TyArrow (yOpt, sa0tye1) sa0tye2 ->
      SA0TyArrow (yOpt, go sa0tye1) $
        case (yOpt, s) of
          (Just y, Subst0 x _) -> if y == x then sa0tye2 else go sa0tye2
          (Nothing, _) -> go sa0tye2
          (_, Subst1 _ _) -> go sa0tye2
    SA0TyCode a1tye1 ->
      SA0TyCode (go a1tye1)
    SA0TyForAll atyvar a0tye ->
      SA0TyForAll atyvar (go a0tye)
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
      (SA0TyForAll atyvar1 sa0tye1', SA0TyForAll atyvar2 sa0tye2') ->
        -- TODO (enhance): true alpha-equivalence
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
        TyEq1Lstm (i1, i2) (h1, h2) -> unionPairs [frees i1, frees i2, frees h1, frees h2]
        TyEq1TextHelper (labels1, labels2) -> unionPairs [frees labels1, frees labels2]
    TyEq1List ty1eqElem ->
      frees ty1eqElem
    TyEq1Maybe ty1eqElem ->
      frees ty1eqElem
    TyEq1Data _datatyId datatyArg1eqs ->
      unionPairs (map frees datatyArg1eqs)
    TyEq1Arrow _labelOpt ty1eqDom ty1eqCod ->
      unionPairs [frees ty1eqDom, frees ty1eqCod]
    TyEq1OmsArrow _label ty1eqDom ty1eqCod ->
      unionPairs [frees ty1eqDom, frees ty1eqCod]
    TyEq1Product ty1eqs ->
      unionPairs (map frees (TwoOrMore.toList ty1eqs))
    TyEq1Record rty1eq ->
      unionPairs (map (frees . snd) (Map.toList rty1eq))
    TyEq1TypeVar _atyvar ->
      (Set.empty, Set.empty)
    TyEq1ForAll _atyvar ty1eq ->
      frees ty1eq

  subst s = \case
    TyEq1Prim ty1eqPrim ->
      TyEq1Prim $
        case ty1eqPrim of
          TyEq1PrimBase tyPrimBase -> TyEq1PrimBase tyPrimBase
          TyEq1Tensor listEq -> TyEq1Tensor (go listEq)
          TyEq1Lstm (i1, i2) (h1, h2) -> TyEq1Lstm (go i1, go i2) (go h1, go h2)
          TyEq1TextHelper (labels1, labels2) -> TyEq1TextHelper (go labels1, go labels2)
    TyEq1List ty1eqElem ->
      TyEq1List (go ty1eqElem)
    TyEq1Maybe ty1eqElem ->
      TyEq1Maybe (go ty1eqElem)
    TyEq1Data datatyId datatyArg1eqs ->
      TyEq1Data datatyId (map go datatyArg1eqs)
    TyEq1Arrow labelOpt ty1eqDom ty1eqCod ->
      TyEq1Arrow labelOpt (go ty1eqDom) (go ty1eqCod)
    TyEq1OmsArrow label ty1eqDom ty1eqCod ->
      TyEq1OmsArrow label (go ty1eqDom) (go ty1eqCod)
    TyEq1Product ty1eqs ->
      TyEq1Product (fmap go ty1eqs)
    TyEq1Record rty1eq ->
      TyEq1Record (fmap go rty1eq)
    TyEq1TypeVar atyvar ->
      TyEq1TypeVar atyvar
    TyEq1ForAll atyvar ty1eq ->
      TyEq1ForAll atyvar (go ty1eq)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent ty1eq1 ty1eq2 =
    case (ty1eq1, ty1eq2) of
      (TyEq1Prim ty1eqPrim1, TyEq1Prim ty1eqPrim2) ->
        case (ty1eqPrim1, ty1eqPrim2) of
          (TyEq1PrimBase tyPrimBase1, TyEq1PrimBase tyPrimBase2) -> tyPrimBase1 == tyPrimBase2
          (TyEq1Tensor listEq1, TyEq1Tensor listEq2) -> go listEq1 listEq2
          (_, _) -> False
      (TyEq1List ty1eqElem1, TyEq1List ty1eqElem2) ->
        go ty1eqElem1 ty1eqElem2
      (TyEq1Maybe ty1eqElem1, TyEq1Maybe ty1eqElem2) ->
        go ty1eqElem1 ty1eqElem2
      (TyEq1Data datatyId1 datatyArg1eqs1, TyEq1Data datatyId2 datatyArg1eqs2) ->
        datatyId1 == datatyId2
          && case zipExactMay datatyArg1eqs1 datatyArg1eqs2 of
            Just zipped -> all (uncurry go) zipped
            Nothing -> False
      (TyEq1Arrow labelOpt1 ty1eqDom1 ty1eqCod1, TyEq1Arrow labelOpt2 ty1eqDom2 ty1eqCod2) ->
        labelOpt1 == labelOpt2 && go ty1eqDom1 ty1eqDom2 && go ty1eqCod1 ty1eqCod2
      (TyEq1OmsArrow label1 ty1eqDom1 ty1eqCod1, TyEq1OmsArrow label2 ty1eqDom2 ty1eqCod2) ->
        label1 == label2 && go ty1eqDom1 ty1eqDom2 && go ty1eqCod1 ty1eqCod2
      (TyEq1Product ty1eqs1, TyEq1Product ty1eqs2) ->
        case zipExactMay (TwoOrMore.toList ty1eqs1) (TwoOrMore.toList ty1eqs2) of
          Just zipped -> all (uncurry go) zipped
          Nothing -> False
      (TyEq1Record rty1eq1, TyEq1Record rty1eq2) ->
        Map.keysSet rty1eq1 == Map.keysSet rty1eq2
          && all (uncurry go . snd) (Map.toList (Map.intersectionWith (,) rty1eq1 rty1eq2))
      (TyEq1TypeVar atyvar1, TyEq1TypeVar atyvar2) ->
        atyvar1 == atyvar2
      (TyEq1ForAll atyvar1 ty1eq1', TyEq1ForAll atyvar2 ty1eq2') ->
        -- TODO (enhance): true alpha-equivalence
        atyvar1 == atyvar2 && go ty1eq1' ty1eq2'
      (_, _) ->
        False
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv -> Bool
      go = alphaEquivalent

instance (Ord sv) => HasVar sv DatatypeArg1EquationF where
  frees = \case
    DatatypeArgEq1Type ty1eq -> frees ty1eq
    DatatypeArgEq1Val0 (a0e1, a0e2) -> unionPairs [frees a0e1, frees a0e2]

  subst s = \case
    DatatypeArgEq1Type ty1eq ->
      DatatypeArgEq1Type (go ty1eq)
    DatatypeArgEq1Val0 (a0e1, a0e2) ->
      DatatypeArgEq1Val0 (go a0e1, go a0e2)
    where
      go :: forall af. (HasVar sv af) => af sv -> af sv
      go = subst s

  alphaEquivalent datatyArg1eq1 datatyArg1eq2 =
    case (datatyArg1eq1, datatyArg1eq2) of
      (DatatypeArgEq1Type ty1eq1, DatatypeArgEq1Type ty1eq2) -> go ty1eq1 ty1eq2
      (DatatypeArgEq1Val0 (a0e11, a0e12), DatatypeArgEq1Val0 (a0e21, a0e22)) -> go a0e11 a0e21 && go a0e12 a0e22
      _ -> False
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
    CastOmsGiven0 cast a0tye r -> unionPairs [frees (Maybe1 cast), frees a0tye, frees r]
    InsertOmitted0 r -> frees r
    CastOmsGiven1 cast a1tye r -> unionPairs [frees (Maybe1 cast), frees a1tye, frees r]
    InsertOmitted1 r -> frees r
    CastInfGiven0 cast a0tye r -> unionPairs [frees (Maybe1 cast), frees a0tye, frees r]
    FillInferred0 a0e r -> unionPairs [frees a0e, frees r]
    InsertInferred0 a0e r -> unionPairs [frees a0e, frees r]
    Instantiated0 r -> frees r
    InsertInferredType0 a0tye r -> unionPairs [frees a0tye, frees r]
    Instantiated1 r -> frees r
    InsertInferredType1 a1tye r -> unionPairs [frees a1tye, frees r]

  subst s = \case
    Pure v -> Pure (go v)
    Cast0 cast a0tye r -> Cast0 (unMaybe1 . go . Maybe1 $ cast) (go a0tye) (go r)
    Cast1 eq a1tye r -> Cast1 (unMaybe1 . go . Maybe1 $ eq) (go a1tye) (go r)
    CastOmsGiven0 cast a0tye r -> CastOmsGiven0 (unMaybe1 . go . Maybe1 $ cast) (go a0tye) (go r)
    InsertOmitted0 r -> InsertOmitted0 (go r)
    CastOmsGiven1 cast a1tye r -> CastOmsGiven1 (unMaybe1 . go . Maybe1 $ cast) (go a1tye) (go r)
    InsertOmitted1 r -> InsertOmitted1 (go r)
    CastInfGiven0 cast a0tye r -> CastInfGiven0 (unMaybe1 . go . Maybe1 $ cast) (go a0tye) (go r)
    FillInferred0 a0e r -> FillInferred0 (go a0e) (go r)
    InsertInferred0 a0e r -> InsertInferred0 (go a0e) (go r)
    Instantiated0 r -> Instantiated0 (go r)
    InsertInferredType0 a0tye r -> InsertInferredType0 (go a0tye) (go r)
    Instantiated1 r -> Instantiated1 (go r)
    InsertInferredType1 a1tye r -> InsertInferredType1 (go a1tye) (go r)
    where
      go :: forall bf. (HasVar sv bf) => bf sv -> bf sv
      go = subst s

  alphaEquivalent _ _ =
    False -- TODO (enhance): Result a, alphaEquivalent
