module Staged.Typechecker.Monad
  ( TypecheckConfig (..),
    TypecheckState (..),
    ImplicitArgLogF (..),
    ImplicitArgLog,
    M,
    M',
    run,
    askConfig,
    getState,
    putState,
    liftEither,
    typeError,
    mapTypeError,
    logImplicitArg,
    generateFreshVar,
    generateFreshTypeVar,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Staged.SrcSyntax
import Staged.Syntax
import Staged.TypeError
import Util.Elaborator
import Util.LocationInFile (SourceSpec, SpanInFile)
import Prelude

data TypecheckConfig = TypecheckConfig
  { optimizeTrivialAssertion :: Bool,
    distributeIfUnderTensorShape :: Bool,
    sourceSpec :: SourceSpec
  }

data TypecheckState = TypecheckState
  { nextVarIndex :: Int,
    assVarDisplay :: Map StaticVar Text,
    nextTypeVarIndex :: Int,
    assTypeVarDisplay :: Map AssTypeVar Text,
    implicitArgLogRev :: [ImplicitArgLog]
  }

data ImplicitArgLogF sv
  = LogGivenArg SpanInFile (Ass0ExprF sv)
  | LogInferredArg SpanInFile (Ass0ExprF sv)
  deriving stock (Functor, Generic)

type ImplicitArgLog = ImplicitArgLogF StaticVar

type M' err trav a = Elaborator TypecheckState TypecheckConfig err trav a

type M trav a = M' TypeError trav a

typeError :: trav -> err -> M' err trav a
typeError = raiseError

mapTypeError :: (err1 -> err2) -> M' err1 trav a -> M' err2 trav a
mapTypeError = mapError

logImplicitArg :: ImplicitArgLog -> M trav ()
logImplicitArg impArgLog = do
  tcState@TypecheckState {implicitArgLogRev} <- getState
  putState $ tcState {implicitArgLogRev = impArgLog : implicitArgLogRev}

generateFreshVar :: Maybe Text -> M' err trav StaticVar
generateFreshVar maybeName = do
  currentState@TypecheckState {nextVarIndex = n, assVarDisplay} <- getState
  let t = fromMaybe (Text.pack ("#X" ++ show n)) maybeName
  let sv = StaticVar n
  putState $ currentState {nextVarIndex = n + 1, assVarDisplay = Map.insert sv t assVarDisplay}
  pure sv

generateFreshTypeVar :: TypeVar -> M' err trav AssTypeVar
generateFreshTypeVar (TypeVar name) = do
  currentState@TypecheckState {nextTypeVarIndex = n, assTypeVarDisplay} <- getState
  let atyvar = AssTypeVar n
  putState $
    currentState
      { nextTypeVarIndex = n + 1,
        assTypeVarDisplay = Map.insert atyvar name assTypeVarDisplay
      }
  pure atyvar
