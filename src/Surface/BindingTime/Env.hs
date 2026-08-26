module Surface.BindingTime.Env
  ( BindingTimeValueEntry (..),
    BindingTimeTypeEntry (..),
    BindingTimeConstructorEntry (..),
    BindingTimeModuleEntry (..),
    BindingTimeEnv,
    empty,
    addVal,
    findVal,
    addType,
    findType,
    addConstructor,
    findConstructor,
    addModule,
    findModule,
    union,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Staged.Core (ConstructorName)
import Surface.BindingTime.Core
import Surface.Syntax (ModuleName, TypeName, Var)
import Prelude

data BindingTimeValueEntry
  = BTValBuiltInPersistent Var (BIPolyTypeF ())
  | BTValBuiltInFixed0 Var BIPolyType
  | BTValBuiltInFixed1 Var BIPolyType
  | BTValLocallyBound BindingTime BIType

data BindingTimeTypeEntry
  = BTType1Alias (BIParameterizedTypeF BindingTimeConst)
  | BTType1Data [BITypeParam]

data BindingTimeConstructorEntry
  = BTCtor [BITypeParam] [BITypeF BindingTimeConst BITypeBoundVar]

newtype BindingTimeModuleEntry
  = BTModule BindingTimeEnv

data BindingTimeEnv = BindingTimeEnv
  { vals :: Map Var BindingTimeValueEntry,
    types :: Map TypeName BindingTimeTypeEntry,
    constructors :: Map ConstructorName BindingTimeConstructorEntry,
    modules :: Map ModuleName BindingTimeModuleEntry
  }

empty :: BindingTimeEnv
empty =
  BindingTimeEnv
    { vals = Map.empty,
      types = Map.empty,
      constructors = Map.empty,
      modules = Map.empty
    }

addVal :: Var -> BindingTimeValueEntry -> BindingTimeEnv -> BindingTimeEnv
addVal x valEntry btenv = btenv {vals = Map.insert x valEntry btenv.vals}

findVal :: Var -> BindingTimeEnv -> Maybe BindingTimeValueEntry
findVal x btenv = Map.lookup x btenv.vals

addType :: TypeName -> BindingTimeTypeEntry -> BindingTimeEnv -> BindingTimeEnv
addType tyName tyEntry btenv = btenv {types = Map.insert tyName tyEntry btenv.types}

findType :: TypeName -> BindingTimeEnv -> Maybe BindingTimeTypeEntry
findType tyName btenv = Map.lookup tyName btenv.types

addConstructor :: ConstructorName -> BindingTimeConstructorEntry -> BindingTimeEnv -> BindingTimeEnv
addConstructor ctor ctorEntry btenv = btenv {constructors = Map.insert ctor ctorEntry btenv.constructors}

findConstructor :: ConstructorName -> BindingTimeEnv -> Maybe BindingTimeConstructorEntry
findConstructor ctor btenv = Map.lookup ctor btenv.constructors

addModule :: ModuleName -> BindingTimeModuleEntry -> BindingTimeEnv -> BindingTimeEnv
addModule m modEntry btenv = btenv {modules = Map.insert m modEntry btenv.modules}

findModule :: ModuleName -> BindingTimeEnv -> Maybe BindingTimeModuleEntry
findModule m btenv = Map.lookup m btenv.modules

union :: BindingTimeEnv -> BindingTimeEnv -> BindingTimeEnv
union btenv1 btenv2 =
  BindingTimeEnv
    { vals = Map.union btenv1.vals btenv2.vals,
      types = Map.union btenv1.types btenv2.types,
      constructors = Map.union btenv1.constructors btenv2.constructors,
      modules = Map.union btenv1.modules btenv2.modules
    }
