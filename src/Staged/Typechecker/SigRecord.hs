module Staged.Typechecker.SigRecord
  ( Ass0Metadata (..),
    Ass1Metadata (..),
    AssPersMetadata (..),
    ValEntry (..),
    Ass1TypeParam (..),
    TypeEntry (..),
    DatatypeEntry (..),
    ConstructorEntry (..),
    ModuleEntry (..),
    SigRecord,
    empty,
    findVal,
    findType,
    findConstructor,
    findModule,
    singletonVal,
    singletonTypeAlias,
    singletonTypeData,
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
import Staged.DatatypeId (DatatypeId)
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

data TypeEntry
  = Ass1TypeAlias [Ass1TypeParam] Ass1TypeExpr
  | Ass1TypeData [Ass1TypeParam] DatatypeId

data DatatypeEntry = DatatypeEntry
  { parameters :: [Ass1TypeParam],
    constructors :: Map ConstructorName [Ass1TypeExpr]
  }

data ConstructorEntry
  = Ass1Constructor [Ass1TypeParam] [Ass1TypeExpr] DatatypeId

newtype ModuleEntry
  = ModuleEntry SigRecord

data SigRecord = SigRecord
  { sigVals :: Map Var ValEntry,
    sigTypes :: Map TypeName TypeEntry,
    sigDatatypes :: Map DatatypeId DatatypeEntry,
    sigConstructors :: Map ConstructorName ConstructorEntry,
    sigModules :: Map ModuleName ModuleEntry
  }

empty :: SigRecord
empty =
  SigRecord
    { sigVals = Map.empty,
      sigTypes = Map.empty,
      sigDatatypes = Map.empty,
      sigConstructors = Map.empty,
      sigModules = Map.empty
    }

findVal :: Var -> SigRecord -> Maybe ValEntry
findVal x sigr = Map.lookup x sigr.sigVals

findType :: TypeName -> SigRecord -> Maybe TypeEntry
findType tyName sigr = Map.lookup tyName sigr.sigTypes

findConstructor :: ConstructorName -> SigRecord -> Maybe ConstructorEntry
findConstructor ctor sigr = Map.lookup ctor sigr.sigConstructors

findModule :: ModuleName -> SigRecord -> Maybe ModuleEntry
findModule m sigr = Map.lookup m sigr.sigModules

singletonVal :: Var -> ValEntry -> SigRecord
singletonVal var entry = empty {sigVals = Map.singleton var entry}

singletonTypeAlias :: TypeName -> [Ass1TypeParam] -> Ass1TypeExpr -> SigRecord
singletonTypeAlias tyName a1tyParams a1tye =
  empty {sigTypes = Map.singleton tyName (Ass1TypeAlias a1tyParams a1tye)}

singletonTypeData :: TypeName -> [Ass1TypeParam] -> DatatypeId -> Map ConstructorName [Ass1TypeExpr] -> SigRecord
singletonTypeData tyName a1tyParams datatyId ctormap =
  empty
    { sigTypes = Map.singleton tyName (Ass1TypeData a1tyParams datatyId),
      sigDatatypes = Map.singleton datatyId (DatatypeEntry a1tyParams ctormap),
      sigConstructors = Map.map (\a1tyes -> Ass1Constructor a1tyParams a1tyes datatyId) ctormap
    }

singletonModule :: ModuleName -> ModuleEntry -> SigRecord
singletonModule m modEntry = empty {sigModules = Map.singleton m modEntry}

intersection :: SigRecord -> SigRecord -> ([Var], [TypeName], [ModuleName])
intersection sigr1 sigr2 =
  ( map fst $ Map.toList (Map.intersection sigr1.sigVals sigr2.sigVals),
    map fst $ Map.toList (Map.intersection sigr1.sigTypes sigr2.sigTypes),
    map fst $ Map.toList (Map.intersection sigr1.sigModules sigr2.sigModules)
  )

-- | Note: prefers `sigr1` for duplicate keys.
union :: SigRecord -> SigRecord -> SigRecord
union sigr1 sigr2 =
  SigRecord
    { sigVals = Map.union sigr1.sigVals sigr2.sigVals,
      sigTypes = Map.union sigr1.sigTypes sigr2.sigTypes,
      sigDatatypes = Map.union sigr1.sigDatatypes sigr2.sigDatatypes,
      sigConstructors = Map.union sigr1.sigConstructors sigr2.sigConstructors,
      sigModules = Map.union sigr1.sigModules sigr2.sigModules
    }

fold :: (Var -> ValEntry -> a -> a) -> (TypeName -> TypeEntry -> a -> a) -> (DatatypeId -> DatatypeEntry -> a -> a) -> (ConstructorName -> ConstructorEntry -> a -> a) -> (ModuleName -> ModuleEntry -> a -> a) -> a -> SigRecord -> a
fold fVal fType fDatatype fCtor fModule acc0 sigr =
  acc5
  where
    SigRecord {sigVals, sigTypes, sigDatatypes, sigConstructors, sigModules} = sigr
    acc1 = Map.foldrWithKey fVal acc0 sigVals
    acc2 = Map.foldrWithKey fType acc1 sigTypes
    acc3 = Map.foldrWithKey fDatatype acc2 sigDatatypes
    acc4 = Map.foldrWithKey fCtor acc3 sigConstructors
    acc5 = Map.foldrWithKey fModule acc4 sigModules
