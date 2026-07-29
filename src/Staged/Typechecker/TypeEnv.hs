module Staged.Typechecker.TypeEnv
  ( TypeEnv,
    TypeVarEntry (..),
    empty,
    addVal,
    addVals,
    findVal,
    addTypeVar,
    findTypeVar,
    addType,
    findType,
    addModule,
    findModule,
    appendSigRecord,
  )
where

import Data.List.Extra (firstJust)
import Data.Map (Map)
import Data.Map qualified as Map
import Staged.SrcSyntax (ModuleName, TypeName, TypeVar, Var)
import Staged.Syntax (AssTypeVar)
import Staged.Typechecker.SigRecord (ModuleEntry, SigRecord, TypeEntry, ValEntry)
import Staged.Typechecker.SigRecord qualified as SigRecord
import Prelude

-- TODO (enhance): optimize internal representation
data TypeEnv = TypeEnv
  { envVals :: [(Var, ValEntry)],
    envTypeVars :: [(TypeVar, TypeVarEntry)],
    envTypes :: [(TypeName, TypeEntry)],
    envModules :: [(ModuleName, ModuleEntry)]
  }

data TypeVarEntry
  = TypeVarEntry0 AssTypeVar
  | TypeVarEntry1 AssTypeVar

empty :: TypeEnv
empty =
  TypeEnv
    { envVals = [],
      envTypeVars = [],
      envTypes = [],
      envModules = []
    }

addVal :: Var -> ValEntry -> TypeEnv -> TypeEnv
addVal x valEntry tyEnv =
  tyEnv {envVals = (x, valEntry) : tyEnv.envVals}

addVals :: Map Var ValEntry -> TypeEnv -> TypeEnv
addVals binders tyEnv =
  foldl' (\tyEnv' (x, valEntry) -> addVal x valEntry tyEnv') tyEnv (Map.toList binders)

findVal :: Var -> TypeEnv -> Maybe ValEntry
findVal x0 tyEnv =
  firstJust
    (\(x, valEntry) -> if x == x0 then Just valEntry else Nothing)
    tyEnv.envVals

-- TODO (enhance): eliminate the duplication of same names
addTypeVar :: TypeVar -> TypeVarEntry -> TypeEnv -> TypeEnv
addTypeVar tyvar tyVarEntry tyEnv =
  tyEnv {envTypeVars = (tyvar, tyVarEntry) : tyEnv.envTypeVars}

findTypeVar :: TypeVar -> TypeEnv -> Maybe TypeVarEntry
findTypeVar tyvar0 tyEnv =
  firstJust
    (\(tyvar, tyVarEntry) -> if tyvar == tyvar0 then Just tyVarEntry else Nothing)
    tyEnv.envTypeVars

addType :: TypeName -> TypeEntry -> TypeEnv -> TypeEnv
addType tyName tyEntry tyEnv =
  tyEnv {envTypes = (tyName, tyEntry) : tyEnv.envTypes}

findType :: TypeName -> TypeEnv -> Maybe TypeEntry
findType tyName0 tyEnv =
  firstJust
    (\(tyName, tyEntry) -> if tyName == tyName0 then Just tyEntry else Nothing)
    tyEnv.envTypes

addModule :: ModuleName -> ModuleEntry -> TypeEnv -> TypeEnv
addModule m modEntry tyEnv =
  tyEnv {envModules = (m, modEntry) : tyEnv.envModules}

findModule :: ModuleName -> TypeEnv -> Maybe ModuleEntry
findModule m0 tyEnv =
  firstJust
    (\(m, modEntry) -> if m == m0 then Just modEntry else Nothing)
    tyEnv.envModules

appendSigRecord :: TypeEnv -> SigRecord -> TypeEnv
appendSigRecord =
  SigRecord.fold addVal addType addModule
