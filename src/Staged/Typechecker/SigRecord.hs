module Staged.Typechecker.SigRecord
  ( Ass0Metadata (..),
    Ass1Metadata (..),
    AssPersMetadata (..),
    ValEntry (..),
    Ass1TypeParam (..),
    Ass1TypeDef (..),
    TypeEntry (..),
    ModuleEntry (..),
    SigRecord,
    empty,
    findVal,
    findType,
    findModule,
    singletonVal,
    singletonType,
    singletonModule,
    intersection,
    union,
    fold,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Staged.BuiltIn.Core
import Staged.Core (ConstructorName)
import Staged.SrcSyntax (ModuleName, TypeName, Var)
import Staged.Syntax
import Surface.Syntax qualified as SurfaceSyntax
import Prelude

data Ass0Metadata = Ass0Metadata
  { ass0builtInName :: BuiltIn,
    ass0surfaceName :: Maybe SurfaceSyntax.Var
  }

data Ass1Metadata = Ass1Metadata
  { ass1builtInName :: Ass1BuiltIn,
    ass1surfaceName :: Maybe SurfaceSyntax.Var
  }

data AssPersMetadata = AssPersMetadata
  { assPbuiltInName :: Ass1BuiltIn,
    assPsurfaceName :: Maybe SurfaceSyntax.Var
  }

data ValEntry
  = Ass0Entry Ass0TypeExpr (Either Ass0Metadata StaticVar)
  | Ass1Entry Ass1TypeExpr (Either Ass1Metadata StaticVar)
  | AssPersEntry AssPersTypeExpr AssPersMetadata

data Ass1TypeParam
  = A1TypeParamType AssTypeVar
  | A1TypeParamVal0 AssVar Ass0TypeExpr

data Ass1TypeDef
  = A1TypeDefAlias Ass1TypeExpr
  | A1TypeDefData DatatypeId (Map ConstructorName (Maybe Ass1TypeExpr))

data TypeEntry
  = Ass1TypeEntry [Ass1TypeParam] Ass1TypeDef

newtype ModuleEntry
  = ModuleEntry SigRecord

data SigRecord = SigRecord
  { sigVals :: Map Var ValEntry,
    sigTypes :: Map TypeName TypeEntry,
    sigModules :: Map ModuleName ModuleEntry
  }

empty :: SigRecord
empty = SigRecord {sigVals = Map.empty, sigTypes = Map.empty, sigModules = Map.empty}

findVal :: Var -> SigRecord -> Maybe ValEntry
findVal x sigr = Map.lookup x sigr.sigVals

findType :: TypeName -> SigRecord -> Maybe TypeEntry
findType tyName sigr = Map.lookup tyName sigr.sigTypes

findModule :: ModuleName -> SigRecord -> Maybe ModuleEntry
findModule m sigr = Map.lookup m sigr.sigModules

singletonVal :: Var -> ValEntry -> SigRecord
singletonVal var entry = empty {sigVals = Map.singleton var entry}

singletonType :: TypeName -> TypeEntry -> SigRecord
singletonType tyName tyEntry = empty {sigTypes = Map.singleton tyName tyEntry}

singletonModule :: ModuleName -> ModuleEntry -> SigRecord
singletonModule m modEntry = empty {sigModules = Map.singleton m modEntry}

intersection :: SigRecord -> SigRecord -> ([Var], [TypeName], [ModuleName])
intersection sigr1 sigr2 =
  ( map fst $ Map.toList (Map.intersection sigr1.sigVals sigr2.sigVals),
    map fst $ Map.toList (Map.intersection sigr1.sigTypes sigr2.sigTypes),
    map fst $ Map.toList (Map.intersection sigr1.sigModules sigr2.sigModules)
  )

union :: SigRecord -> SigRecord -> SigRecord
union sigr1 sigr2 =
  SigRecord
    { sigVals = Map.union sigr1.sigVals sigr2.sigVals,
      sigTypes = Map.union sigr1.sigTypes sigr2.sigTypes,
      sigModules = Map.union sigr1.sigModules sigr2.sigModules
    }

fold :: (Var -> ValEntry -> a -> a) -> (TypeName -> TypeEntry -> a -> a) -> (ModuleName -> ModuleEntry -> a -> a) -> a -> SigRecord -> a
fold fVal fType fModule acc0 (SigRecord {sigVals, sigTypes, sigModules}) =
  acc3
  where
    acc1 = Map.foldrWithKey fVal acc0 sigVals
    acc2 = Map.foldrWithKey fType acc1 sigTypes
    acc3 = Map.foldrWithKey fModule acc2 sigModules
