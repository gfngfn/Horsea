module Staged.Scope.SigRecord
  ( Ass0Metadata (..),
    Ass1Metadata (..),
    AssPersMetadata (..),
    ValEntry (..),
    ModuleEntry (..),
    SigRecord,
    empty,
    findVal,
    findModule,
    singletonVal,
    singletonModule,
    intersection,
    union,
    fold,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Staged.BuiltIn.Core
import Staged.SrcSyntax (Var)
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

newtype ModuleEntry
  = ModuleEntry SigRecord

data SigRecord = SigRecord
  { sigVals :: Map Var ValEntry,
    sigModules :: Map Var ModuleEntry
  }

empty :: SigRecord
empty = SigRecord Map.empty Map.empty

findVal :: Var -> SigRecord -> Maybe ValEntry
findVal x (SigRecord vals _) = Map.lookup x vals

findModule :: Var -> SigRecord -> Maybe ModuleEntry
findModule m (SigRecord _ modules) = Map.lookup m modules

singletonVal :: Var -> ValEntry -> SigRecord
singletonVal var entry = SigRecord (Map.singleton var entry) Map.empty

singletonModule :: Var -> ModuleEntry -> SigRecord
singletonModule m modEntry = SigRecord Map.empty (Map.singleton m modEntry)

intersection :: SigRecord -> SigRecord -> ([Var], [Var])
intersection (SigRecord vals1 modules1) (SigRecord vals2 modules2) =
  (map fst $ Map.toList (Map.intersection vals1 vals2), map fst $ Map.toList (Map.intersection modules1 modules2))

union :: SigRecord -> SigRecord -> SigRecord
union (SigRecord vals1 modules1) (SigRecord vals2 modules2) =
  SigRecord (Map.union vals1 vals2) (Map.union modules1 modules2)

fold :: (Var -> ValEntry -> a -> a) -> (Var -> ModuleEntry -> a -> a) -> a -> SigRecord -> a
fold fVal fModule acc (SigRecord {sigVals, sigModules}) =
  Map.foldrWithKey fModule (Map.foldrWithKey fVal acc sigVals) sigModules
