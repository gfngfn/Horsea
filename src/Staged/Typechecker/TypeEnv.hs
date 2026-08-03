module Staged.Typechecker.TypeEnv
  ( DatatypeEnv,
    TypeEnv,
    TypeVarEntry (..),
    empty,
    addVal,
    addVals,
    findVal,
    addTypeVar,
    findTypeVar,
    addType,
    findType,
    addDatatype,
    findDatatype,
    datatypeOnly,
    addConstructor,
    findConstructor,
    addModule,
    findModule,
    appendSigRecord,
  )
where

import Data.List.Extra (firstJust)
import Data.Map (Map)
import Data.Map qualified as Map
import Staged.Core (ConstructorName)
import Staged.DatatypeId (DatatypeId)
import Staged.SrcSyntax (ModuleName, TypeName, TypeVar, Var)
import Staged.Syntax (AssTypeVar)
import Staged.Typechecker.SigRecord (ConstructorEntry, DatatypeEntry, ModuleEntry, SigRecord, TypeEntry, ValEntry)
import Staged.Typechecker.SigRecord qualified as SigRecord
import Prelude

type DatatypeEnv = Map DatatypeId DatatypeEntry

-- TODO (enhance): optimize internal representation
data TypeEnv = TypeEnv
  { envVals :: [(Var, ValEntry)],
    envTypeVars :: [(TypeVar, TypeVarEntry)],
    envTypes :: [(TypeName, TypeEntry)],
    envDatatypes :: DatatypeEnv,
    envConstructors :: [(ConstructorName, ConstructorEntry)],
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
      envDatatypes = Map.empty,
      envConstructors = [],
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

addDatatype :: DatatypeId -> DatatypeEntry -> TypeEnv -> TypeEnv
addDatatype datatyId ctormap tyEnv =
  tyEnv {envDatatypes = Map.insert datatyId ctormap tyEnv.envDatatypes}

findDatatype :: DatatypeId -> TypeEnv -> Maybe DatatypeEntry
findDatatype datatyId tyEnv =
  Map.lookup datatyId tyEnv.envDatatypes

datatypeOnly :: TypeEnv -> DatatypeEnv
datatypeOnly = (.envDatatypes)

addConstructor :: ConstructorName -> ConstructorEntry -> TypeEnv -> TypeEnv
addConstructor ctor ctorEntry tyEnv =
  tyEnv {envConstructors = (ctor, ctorEntry) : tyEnv.envConstructors}

findConstructor :: ConstructorName -> TypeEnv -> Maybe ConstructorEntry
findConstructor ctor0 tyEnv =
  firstJust
    (\(ctor, ctorEntry) -> if ctor == ctor0 then Just ctorEntry else Nothing)
    tyEnv.envConstructors

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
  SigRecord.fold addVal addType addDatatype addConstructor addModule
