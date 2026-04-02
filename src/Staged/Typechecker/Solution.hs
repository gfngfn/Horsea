module Staged.Typechecker.Solution
  ( VarSolution,
    TypeVar0Solution,
    TypeVar1Solution,
    applyVarSolution,
    applyTypeVar0Solution,
    applyTypeVar1Solution,
    composeVarSolution,
    composeTypeVar0Solution,
    composeTypeVar1Solution,
    applySolution0,
    applySolution1,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Staged.Subst
import Staged.Syntax
import Staged.TypeSubst
import Prelude

type VarSolution = Map AssVar (Ass0Expr, Ass0TypeExpr)

type TypeVar0Solution = Map AssTypeVar Ass0TypeExpr

type TypeVar1Solution = Map AssTypeVar Ass1TypeExpr

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

applySolution0 :: forall af. (HasVar StaticVar af, HasTypeVar af) => VarSolution -> TypeVar0Solution -> af StaticVar -> af StaticVar
applySolution0 varSolution tyvar0Solution entity =
  applyTypeVar0Solution tyvar0Solution (applyVarSolution varSolution entity)

applySolution1 :: forall af. (HasVar StaticVar af, HasTypeVar af) => VarSolution -> TypeVar1Solution -> af StaticVar -> af StaticVar
applySolution1 varSolution tyvar1Solution entity =
  applyTypeVar1Solution tyvar1Solution (applyVarSolution varSolution entity)
