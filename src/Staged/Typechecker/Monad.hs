module Staged.Typechecker.Monad
  ( TypecheckConfig (..),
    TypecheckState (..),
    InferableArgLogF (..),
    InferableArgLog,
    ShapeAnnotLog (..),
    M,
    M',
    run,
    askConfig,
    getState,
    putState,
    liftEither,
    typeError,
    mapTypeError,
    logInferableArg,
    logShapeAnnot,
    generateFreshVar,
    generateFreshTypeVar,
    generateFreshDatatypeId,
    makeIdentityLam,
    askSpanInFile,
  )
where

import Common.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Common.TokenUtil (Span)
import Control.Monad.Elaborator
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Staged.SrcSyntax
import Staged.Syntax
import Staged.TypeError
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
    nextDatatypeIndex :: Int,
    datatypeDisplay :: Map DatatypeId Text,
    inferableArgLogRev :: [InferableArgLog],
    shapeAnnotLogRev :: [ShapeAnnotLog]
  }

data InferableArgLogF sv
  = LogGivenArg SpanInFile (Ass0ExprF sv)
  | LogInferredArg SpanInFile (Ass0ExprF sv)
  deriving stock (Functor, Generic)

type InferableArgLog = InferableArgLogF StaticVar

newtype ShapeAnnotLog = ShapeAnnotLog Span

type M' err trav a = Elaborator TypecheckState TypecheckConfig err trav a

type M trav a = M' TypeError trav a

typeError :: trav -> err -> M' err trav a
typeError = raiseError

mapTypeError :: (err1 -> err2) -> M' err1 trav a -> M' err2 trav a
mapTypeError = mapError

logInferableArg :: InferableArgLog -> M trav ()
logInferableArg impArgLog = do
  tcState@TypecheckState {inferableArgLogRev} <- getState
  putState $ tcState {inferableArgLogRev = impArgLog : inferableArgLogRev}

logShapeAnnot :: ShapeAnnotLog -> M trav ()
logShapeAnnot shapeAnnotLog = do
  tcState@TypecheckState {shapeAnnotLogRev} <- getState
  putState $ tcState {shapeAnnotLogRev = shapeAnnotLog : shapeAnnotLogRev}

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

generateFreshDatatypeId :: TypeName -> M' err trav DatatypeId
generateFreshDatatypeId tyName = do
  currentState@TypecheckState {nextDatatypeIndex = n, datatypeDisplay} <- getState
  let datatyId = DatatypeId n
  putState $
    currentState
      { nextDatatypeIndex = n + 1,
        datatypeDisplay = Map.insert datatyId tyName datatypeDisplay
      }
  pure datatyId

makeIdentityLam :: Ass0TypeExpr -> M trav Ass0Expr
makeIdentityLam a0tye = do
  sv <- generateFreshVar Nothing
  let ax = AssVarStatic sv
  pure $ A0Lam Nothing (ax, strictify a0tye) (A0Var ax)

askSpanInFile :: Span -> M trav SpanInFile
askSpanInFile loc = do
  TypecheckConfig {sourceSpec} <- askConfig
  pure $ getSpanInFile sourceSpec loc
