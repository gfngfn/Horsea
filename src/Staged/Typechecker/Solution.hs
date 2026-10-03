module Staged.Typechecker.Solution
  ( VarSolution,
    TypeVar0Solution,
    TypeVar1Solution,
    Solution0 (..),
    Solution1 (..),
    SetToInfer0 (..),
    SetToInfer1 (..),
    applyVarSolution,
    applyTypeVar0Solution,
    applyTypeVar1Solution,
    composeVarSolution,
    composeTypeVar0Solution,
    composeTypeVar1Solution,
    applySolution0,
    applySolution1,
    composeSolution0,
    composeSolution1,
    addVarToSet0,
    addTypeVar0ToSet0,
    addTypeVar1ToSet0,
    deleteSolutionFromSet0,
    deleteSolutionFromSet1,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Staged.Subst
import Staged.Syntax
import Staged.TypeSubst
import Prelude

type VarSolution = Map AssVar (Ass0Expr, Ass0TypeExpr)

type TypeVar0Solution = Map AssTypeVar Ass0TypeExpr

type TypeVar1Solution = Map AssTypeVar Ass1TypeExpr

data Solution0 = Solution0 VarSolution TypeVar0Solution TypeVar1Solution

data Solution1 = Solution1 VarSolution TypeVar1Solution

data SetToInfer0 = SetToInfer0 (Set AssVar) (Set AssTypeVar) (Set AssTypeVar)

data SetToInfer1 = SetToInfer1 (Set AssVar) (Set AssTypeVar)

applyVarSolution :: forall af. (HasVar StaticVar af) => VarSolution -> af StaticVar -> af StaticVar
applyVarSolution varSolution entity =
  Map.foldrWithKey (flip subst0) entity (Map.map fst varSolution)

applyTypeVar0Solution :: forall af. (HasTypeVar af) => TypeVar0Solution -> af StaticVar -> af StaticVar
applyTypeVar0Solution tyvar0Solution entity =
  Map.foldrWithKey (flip tySubst0) entity tyvar0Solution

applyTypeVar1Solution :: forall af. (HasTypeVar af) => TypeVar1Solution -> af StaticVar -> af StaticVar
applyTypeVar1Solution tyvar1Solution entity =
  Map.foldrWithKey (flip tySubst1) entity tyvar1Solution

composeVarSolution :: VarSolution -> VarSolution -> VarSolution
composeVarSolution solNew solOld =
  Map.union
    solNew
    (Map.map (\(a0e, a0tye) -> (applyVarSolution solNew a0e, applyVarSolution solNew a0tye)) solOld)

composeTypeVar0Solution :: TypeVar0Solution -> TypeVar0Solution -> TypeVar0Solution
composeTypeVar0Solution solNew solOld =
  Map.union solNew (Map.map (applyTypeVar0Solution solNew) solOld)

composeTypeVar1Solution :: TypeVar1Solution -> TypeVar1Solution -> TypeVar1Solution
composeTypeVar1Solution solNew solOld =
  Map.union solNew (Map.map (applyTypeVar1Solution solNew) solOld)

applySolution0 :: forall af. (HasVar StaticVar af, HasTypeVar af) => Solution0 -> af StaticVar -> af StaticVar
applySolution0 (Solution0 varSolution tyvar0Solution tyvar1Solution) =
  applyTypeVar1Solution tyvar1Solution . applyTypeVar0Solution tyvar0Solution . applyVarSolution varSolution

applySolution1 :: forall af. (HasVar StaticVar af, HasTypeVar af) => Solution1 -> af StaticVar -> af StaticVar
applySolution1 (Solution1 varSolution tyvar1Solution) entity =
  applyTypeVar1Solution tyvar1Solution (applyVarSolution varSolution entity)

composeSolution0 :: Solution0 -> Solution0 -> Solution0
composeSolution0 sol1 sol2 =
  Solution0
    (composeVarSolution varSolution1 varSolution2)
    (composeTypeVar0Solution tyvar0Solution1 tyvar0Solution2)
    (composeTypeVar1Solution tyvar1Solution1 tyvar1Solution2)
  where
    Solution0 varSolution1 tyvar0Solution1 tyvar1Solution1 = sol1
    Solution0 varSolution2 tyvar0Solution2 tyvar1Solution2 = sol2

composeSolution1 :: Solution1 -> Solution1 -> Solution1
composeSolution1 sol1 sol2 =
  Solution1
    (composeVarSolution varSolution1 varSolution2)
    (composeTypeVar1Solution tyvar1Solution1 tyvar1Solution2)
  where
    Solution1 varSolution1 tyvar1Solution1 = sol1
    Solution1 varSolution2 tyvar1Solution2 = sol2

addVarToSet0 :: AssVar -> SetToInfer0 -> SetToInfer0
addVarToSet0 x (SetToInfer0 varsToInfer tyvars0ToInfer tyvars1ToInfer) =
  SetToInfer0 (Set.insert x varsToInfer) tyvars0ToInfer tyvars1ToInfer

addTypeVar0ToSet0 :: AssTypeVar -> SetToInfer0 -> SetToInfer0
addTypeVar0ToSet0 tyvar (SetToInfer0 varsToInfer tyvars0ToInfer tyvars1ToInfer) =
  SetToInfer0 varsToInfer (Set.insert tyvar tyvars0ToInfer) tyvars1ToInfer

addTypeVar1ToSet0 :: AssTypeVar -> SetToInfer0 -> SetToInfer0
addTypeVar1ToSet0 tyvar (SetToInfer0 varsToInfer tyvars0ToInfer tyvars1ToInfer) =
  SetToInfer0 varsToInfer tyvars0ToInfer (Set.insert tyvar tyvars1ToInfer)

deleteSolutionFromSet0 :: SetToInfer0 -> Solution0 -> SetToInfer0
deleteSolutionFromSet0 setToInfer solution =
  SetToInfer0
    (varsToInfer \\ Map.keysSet varSolution)
    (tyvars0ToInfer \\ Map.keysSet tyvar0Solution)
    (tyvars1ToInfer \\ Map.keysSet tyvar1Solution)
  where
    Solution0 varSolution tyvar0Solution tyvar1Solution = solution
    SetToInfer0 varsToInfer tyvars0ToInfer tyvars1ToInfer = setToInfer

deleteSolutionFromSet1 :: SetToInfer1 -> Solution1 -> SetToInfer1
deleteSolutionFromSet1 setToInfer solution =
  SetToInfer1
    (varsToInfer \\ Map.keysSet varSolution)
    (tyvars1ToInfer \\ Map.keysSet tyvar1Solution)
  where
    Solution1 varSolution tyvar1Solution = solution
    SetToInfer1 varsToInfer tyvars1ToInfer = setToInfer
