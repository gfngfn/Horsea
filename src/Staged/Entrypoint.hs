module Staged.Entrypoint
  ( Argument (..),
    showVar,
    typecheckModuleFile,
    typecheckAndEvalInput,
    handle,
  )
where

import Common.FailureReason (FailureReason (..))
import Common.Formatter (Disp)
import Common.Formatter qualified as Formatter
import Common.LocationInFile (SourceSpec (SourceSpec))
import Common.LocationInFile qualified as LocationInFile
import Control.Lens ((^?))
import Control.Monad (foldM, forM_, unless)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Either.Extra (mapLeft)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Text.IO.Util (readFileEither)
import Data.Tuple.Extra (first)
import Staged.Evaluator qualified as Evaluator
import Staged.Parser qualified as Parser
import Staged.SrcSyntax
import Staged.Syntax
import Staged.TypeError (TypeError)
import Staged.Typechecker qualified as Typechecker
import Staged.Typechecker.Monad (InferableArgLogF (..), ShapeAnnotLog (..), TypecheckConfig (..), TypecheckState (..))
import Staged.Typechecker.SigRecord (SigRecord)
import Staged.Typechecker.TypeEnv (TypeEnv)
import Staged.Typechecker.TypeEnv qualified as TypeEnv
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    moduleFilePaths :: [String],
    insertTrivial :: Bool,
    suppressIfDistribution :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool,
    showParsed :: Bool,
    showElaborated :: Bool,
    showInferred :: Bool,
    statsOnly :: Bool
  }
  deriving (Read, Show)

type M a = ReaderT Argument IO a

success :: a -> M (Either err a)
success = pure . Right

failure :: err -> M (Either err a)
failure = pure . Left

makeConfig :: SourceSpec -> M TypecheckConfig
makeConfig sourceSpec = do
  Argument {insertTrivial, suppressIfDistribution} <- ask
  pure $
    TypecheckConfig
      { optimizeTrivialAssertion = not insertTrivial,
        distributeIfUnderTensorShape = not suppressIfDistribution,
        sourceSpec = sourceSpec
      }

putNormalLine :: String -> M ()
putNormalLine = lift . putStrLn

putSectionLine :: String -> M ()
putSectionLine s =
  lift $ putStrLn ("-------- " ++ s ++ " --------")

putRenderedLines :: (Disp a) => a -> M ()
putRenderedLines v = do
  Argument {displayWidth} <- ask
  lift $ Formatter.putRenderedLines displayWidth v

putRenderedLinesAtStage0 :: (Disp a) => a -> M ()
putRenderedLinesAtStage0 v = do
  Argument {displayWidth} <- ask
  lift $ Formatter.putRenderedLinesAtStage0 displayWidth v

putRenderedLinesAtStage1 :: (Disp a) => a -> M ()
putRenderedLinesAtStage1 v = do
  Argument {displayWidth} <- ask
  lift $ Formatter.putRenderedLinesAtStage1 displayWidth v

typecheckModuleFile :: TypecheckState -> TypeEnv -> SourceSpec -> [Bind] -> M (Either TypeError (TypeEnv, SigRecord, [AssBind]), TypecheckState)
typecheckModuleFile tcState tyEnv sourceSpec binds = do
  tcConfig <- makeConfig sourceSpec
  pure $
    first (mapLeft fst) $
      Typechecker.run (Typechecker.typecheckBinds () tyEnv binds) tcConfig tcState

typecheckInput :: SourceSpec -> TypecheckState -> TypeEnv -> Expr -> M (Either TypeError (ResultF Ass0TypeExprF StaticVar, Ass0Expr), TypecheckState)
typecheckInput sourceSpecOfInput tcState tyEnvStub e = do
  tcConfig <- makeConfig sourceSpecOfInput
  pure $
    first (mapLeft fst) $
      Typechecker.run (Typechecker.typecheckExpr0 () tyEnvStub [] e) tcConfig tcState

showVar :: Map StaticVar Text -> StaticVar -> Text
showVar assVarDisplay sv =
  fromMaybe "<!!UNKNOWN-VAR!!>" (Map.lookup sv assVarDisplay)

putSkipped :: String -> M ()
putSkipped option =
  putNormalLine $ "  Skipped; specify " ++ option ++ " to see this"

displayParsed :: Expr -> M ()
displayParsed e = do
  Argument {statsOnly, showParsed} <- ask
  unless statsOnly $ do
    putSectionLine "parsed expression:"
    if showParsed
      then putRenderedLinesAtStage0 e
      else putSkipped "--show-parsed"

displayElaborated :: Map StaticVar Text -> Result0 -> Ass0Expr -> M ()
displayElaborated assVarDisplay result a0e = do
  Argument {statsOnly, showElaborated} <- ask
  unless statsOnly $ do
    putSectionLine "type:"
    if showElaborated
      then putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) result)
      else putSkipped "--show-elaborated"
    putSectionLine "elaborated expression:"
    if showElaborated
      then putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) a0e)
      else putSkipped "--show-elaborated"

displayInferenceResult :: [InferableArgLogF Text] -> M ()
displayInferenceResult impArgLogs = do
  Argument {statsOnly, showInferred} <- ask
  unless statsOnly $ do
    putSectionLine "inference result:"
    if showInferred
      then forM_ impArgLogs putRenderedLines
      else putSkipped "--show-inferred"

displayGenerated :: Map StaticVar Text -> Ass1Val -> M ()
displayGenerated assVarDisplay a1v = do
  Argument {statsOnly} <- ask
  unless statsOnly $ do
    putSectionLine "generated code:"
    putRenderedLinesAtStage1 (fmap (showVar assVarDisplay) a1v)

displayStats :: [InferableArgLogF Text] -> [ShapeAnnotLog] -> M ()
displayStats impArgLogs shapeAnnotLogs = do
  putSectionLine "stats:"
  putNormalLine $ "- Implicit arguments: total = " ++ show numTotal ++ ", inferred = " ++ show numInferred
  putNormalLine $ "- Number of shapes in type annotations: " ++ show (length shapeAnnotLogs)
  where
    numTotal = length impArgLogs
    numInferred = length $ filter (isJust . (^? #_LogInferredArg)) impArgLogs

typecheckAndEvalInput :: TypecheckState -> TypeEnv -> [AssBind] -> SourceSpec -> Expr -> M (Either FailureReason ())
typecheckAndEvalInput tcStateAfterStub tyEnvStub abinds sourceSpecOfInput e = do
  Argument {compileTimeOnly} <- ask
  let tcState = tcStateAfterStub {inferableArgLogRev = [], shapeAnnotLogRev = []}
  (r, TypecheckState {assVarDisplay, inferableArgLogRev, shapeAnnotLogRev}) <-
    typecheckInput sourceSpecOfInput tcState tyEnvStub e
  let implicitArgLog = map (fmap (showVar assVarDisplay)) $ reverse inferableArgLogRev
  let shapeAnnotLog = reverse shapeAnnotLogRev
  case r of
    Left tyErr -> do
      putSectionLine "type error:"
      putRenderedLines (fmap (showVar assVarDisplay) tyErr)
      failure ExitByTypeError
    Right (result, a0eWithoutStub) -> do
      let a0e = makeExprFromBinds abinds a0eWithoutStub
      displayElaborated assVarDisplay result a0e
      displayInferenceResult implicitArgLog
      let initialEvalState = Evaluator.initialState sourceSpecOfInput
      case Evaluator.run (Evaluator.evalExpr0 initialEnv a0e) initialEvalState of
        Left err -> do
          putSectionLine "error during compile-time code generation:"
          putRenderedLines (fmap (showVar assVarDisplay) err)
          failure ExitByCompileTimeEvalError
        Right a0v -> do
          case a0v of
            A0ValBracket a1v -> do
              displayGenerated assVarDisplay a1v
              displayStats implicitArgLog shapeAnnotLog
              if compileTimeOnly
                then success ()
                else do
                  let a0eRuntime = Evaluator.unliftVal a1v
                  case Evaluator.run (Evaluator.evalExpr0 initialEnv a0eRuntime) initialEvalState of
                    Left err -> do
                      putSectionLine "eval error:"
                      putRenderedLines (fmap (showVar assVarDisplay) err)
                      failure ExitByRuntimeEvalError
                    Right a0vRuntime -> do
                      putSectionLine "result of runtime evaluation:"
                      putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) a0vRuntime)
                      success ()
            _ -> do
              putSectionLine "stage-0 result:"
              putNormalLine "(The stage-0 result was not a code value)"
              putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) a0v)
              if compileTimeOnly
                then success ()
                else failure ExitByRuntimeEvalError
  where
    initialEnv :: EvalEnv
    initialEnv = EvalEnv {vals = Map.empty, typeVals = Map.empty}

typecheckAndEval :: [(SourceSpec, [Bind])] -> SourceSpec -> Expr -> M (Either FailureReason ())
typecheckAndEval modules sourceSpecOfInput e = do
  let tcStateInit =
        TypecheckState
          { nextVarIndex = 0,
            assVarDisplay = Map.empty,
            nextTypeVarIndex = 0,
            assTypeVarDisplay = Map.empty,
            nextDatatypeIndex = 0,
            inferableArgLogRev = [],
            shapeAnnotLogRev = []
          }
      tyEnvInit = TypeEnv.empty
  r_ <-
    foldM
      ( \acc_ (sourceSpec, binds) -> do
          case acc_ of
            Left err ->
              failure err
            Right (tcState0, tyEnv0, abinds0) -> do
              (r, tcState1@TypecheckState {assVarDisplay}) <-
                typecheckModuleFile tcState0 tyEnv0 sourceSpec binds
              case r of
                Left tyErr -> do
                  putSectionLine "type error in a module file:"
                  putRenderedLines (fmap (showVar assVarDisplay) tyErr)
                  failure ExitByTypeError
                Right (tyEnv1, _sigrMod, abindsMod) -> do
                  let abinds1 = abinds0 ++ abindsMod
                  success (tcState1, tyEnv1, abinds1)
      )
      (Right (tcStateInit, tyEnvInit, []))
      modules
  case r_ of
    Left err -> failure err
    Right (tcState, tyEnv, abinds) -> typecheckAndEvalInput tcState tyEnv abinds sourceSpecOfInput e

readModuleFile :: FilePath -> M (Either FailureReason (SourceSpec, [Bind]))
readModuleFile moduleFilePath = do
  contents_ <- lift $ readFileEither moduleFilePath
  case contents_ of
    Left err -> do
      putNormalLine $ "IO error: " ++ err
      failure ExitByIOError
    Right contents -> do
      let sourceSpec =
            SourceSpec
              { LocationInFile.source = contents,
                LocationInFile.inputFilePath = moduleFilePath
              }
      case Parser.parseBinds sourceSpec contents of
        Left err -> do
          putSectionLine "parse error of a module file:"
          putRenderedLines err
          failure ExitByParseError
        Right binds -> do
          success (sourceSpec, binds)

readExprFile :: FilePath -> M (Either FailureReason (SourceSpec, Expr))
readExprFile exprFilePath = do
  contents_ <- lift $ readFileEither exprFilePath
  case contents_ of
    Left err -> do
      putNormalLine $ "IO error: " ++ err
      failure ExitByIOError
    Right contents -> do
      let sourceSpec =
            SourceSpec
              { LocationInFile.source = contents,
                LocationInFile.inputFilePath = exprFilePath
              }
      case Parser.parseExpr sourceSpec contents of
        Left err -> do
          putSectionLine "parse error of an expression file:"
          putRenderedLines err
          failure ExitByParseError
        Right e -> do
          success (sourceSpec, e)

handle' :: M (Either FailureReason ())
handle' = do
  Argument {inputFilePath, moduleFilePaths} <- ask
  putNormalLine "Staged Shape-Dependent Types (Lambda-Bracket-Assertion)"
  moduleAcc_ <-
    foldM
      ( \moduleAcc0_ moduleFilePath -> do
          case moduleAcc0_ of
            Left err ->
              failure err
            Right moduleAcc -> do
              module_ <- readModuleFile moduleFilePath
              case module_ of
                Left err -> failure err
                Right modul -> success $ modul : moduleAcc
      )
      (Right [])
      moduleFilePaths
  case moduleAcc_ of
    Left err -> do
      failure err
    Right moduleAcc -> do
      let modules = reverse moduleAcc
      source_ <- readExprFile inputFilePath
      case source_ of
        Left err -> do
          failure err
        Right (sourceSpecOfInput, e) -> do
          displayParsed e
          typecheckAndEval modules sourceSpecOfInput e

-- Returns a boolean that represents success or failure
handle :: Argument -> IO (Either FailureReason ())
handle = runReaderT handle'
