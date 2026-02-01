module Staged.Typechecker.TypeEnv
  ( TypeEnv,
    TypeVarEntry (..),
    empty,
    addVal,
    findVal,
    addTypeVar,
    findTypeVar,
    addModule,
    findModule,
    appendSigRecord,
  )
where

import Data.List.Extra qualified as List
import Staged.Typechecker.SigRecord (ModuleEntry, SigRecord, ValEntry)
import Staged.Typechecker.SigRecord qualified as SigRecord
import Staged.SrcSyntax (TypeVar, Var)
import Staged.Syntax (AssTypeVar)
import Prelude

-- TODO (enhance): optimize internal representation
data TypeEnv = TypeEnv
  { envVals :: [(Var, ValEntry)],
    envTypeVars :: [(TypeVar, TypeVarEntry)],
    envModules :: [(Var, ModuleEntry)]
  }

data TypeVarEntry
  = TypeVarEntry0 AssTypeVar
  | TypeVarEntry1 AssTypeVar

empty :: TypeEnv
empty = TypeEnv {envVals = [], envTypeVars = [], envModules = []}

addVal :: Var -> ValEntry -> TypeEnv -> TypeEnv
addVal x valEntry tyEnv =
  tyEnv {envVals = (x, valEntry) : tyEnv.envVals}

findVal :: Var -> TypeEnv -> Maybe ValEntry
findVal x0 tyEnv =
  List.firstJust
    (\(x, valEntry) -> if x == x0 then Just valEntry else Nothing)
    tyEnv.envVals

-- TODO: eliminate the duplication of same names
addTypeVar :: TypeVar -> TypeVarEntry -> TypeEnv -> TypeEnv
addTypeVar tyvar tyVarEntry tyEnv =
  tyEnv {envTypeVars = (tyvar, tyVarEntry) : tyEnv.envTypeVars}

findTypeVar :: TypeVar -> TypeEnv -> Maybe TypeVarEntry
findTypeVar tyvar0 tyEnv =
  List.firstJust
    (\(tyvar, tyVarEntry) -> if tyvar == tyvar0 then Just tyVarEntry else Nothing)
    tyEnv.envTypeVars

addModule :: Var -> ModuleEntry -> TypeEnv -> TypeEnv
addModule m modEntry tyEnv =
  tyEnv {envModules = (m, modEntry) : tyEnv.envModules}

findModule :: Var -> TypeEnv -> Maybe ModuleEntry
findModule m0 tyEnv =
  List.firstJust
    (\(m, modEntry) -> if m == m0 then Just modEntry else Nothing)
    tyEnv.envModules

appendSigRecord :: TypeEnv -> SigRecord -> TypeEnv
appendSigRecord =
  SigRecord.fold addVal addModule
