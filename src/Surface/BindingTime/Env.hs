module Surface.BindingTime.Env
  ( BindingTimeValueEntry (..),
    BindingTimeModuleEntry (..),
    BindingTimeEnv,
    empty,
    addVal,
    findVal,
    addModule,
    findModule,
    union,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Surface.BindingTime.Core
import Surface.Syntax (ModuleName, Var)
import Prelude

data BindingTimeValueEntry
  = BTValBuiltInPersistent Var (BIPolyTypeF ())
  | BTValBuiltInFixed0 Var BIPolyTypeVoid
  | BTValBuiltInFixed1 Var BITypeVoid
  | BTValLocallyBound BindingTime BIType

newtype BindingTimeModuleEntry
  = BTModule BindingTimeEnv

data BindingTimeEnv = BindingTimeEnv
  { vals :: Map Var BindingTimeValueEntry,
    modules :: Map ModuleName BindingTimeModuleEntry
  }

empty :: BindingTimeEnv
empty =
  BindingTimeEnv
    { vals = Map.empty,
      modules = Map.empty
    }

addVal :: Var -> BindingTimeValueEntry -> BindingTimeEnv -> BindingTimeEnv
addVal x valEntry btenv = btenv {vals = Map.insert x valEntry btenv.vals}

findVal :: Var -> BindingTimeEnv -> Maybe BindingTimeValueEntry
findVal x btenv = Map.lookup x btenv.vals

addModule :: ModuleName -> BindingTimeModuleEntry -> BindingTimeEnv -> BindingTimeEnv
addModule m modEntry btenv = btenv {modules = Map.insert m modEntry btenv.modules}

findModule :: ModuleName -> BindingTimeEnv -> Maybe BindingTimeModuleEntry
findModule m btenv = Map.lookup m btenv.modules

union :: BindingTimeEnv -> BindingTimeEnv -> BindingTimeEnv
union btenv1 btenv2 =
  BindingTimeEnv
    { vals = Map.union btenv1.vals btenv2.vals,
      modules = Map.union btenv1.modules btenv2.modules
    }
