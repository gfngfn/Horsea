module Staged.Entrypoint
  ( Argument (..),
    showVar,
    typecheckStub,
    typecheckAndEvalInput,
    handle,
  )
where

import Control.Lens ((^?))
import Control.Monad (forM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.Either.Extra (mapLeft)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import Data.Tuple.Extra (first)
import Staged.Evaluator qualified as Evaluator
import Staged.Formatter (Disp)
import Staged.Formatter qualified as Formatter
import Staged.Parser qualified as Parser
import Staged.SrcSyntax
import Staged.Syntax
import Staged.TypeError (TypeError)
import Staged.Typechecker qualified as Typechecker
import Staged.Typechecker.Monad (ImplicitArgLogF (..), TypecheckConfig (..), TypecheckState (..))
import Staged.Typechecker.SigRecord (SigRecord)
import Staged.Typechecker.TypeEnv (TypeEnv)
import Staged.Typechecker.TypeEnv qualified as TypeEnv
import Util.FailureReason (FailureReason (..))
import Util.IO (readFileEither)
import Util.LocationInFile (SourceSpec (SourceSpec))
import Util.LocationInFile qualified as LocationInFile
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    stubFilePath :: String,
    insertTrivial :: Bool,
    suppressIfDistribution :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool,
    showParsed :: Bool,
    showElaborated :: Bool,
    showInferred :: Bool
  }
  deriving (Read, Show)

type M a = ReaderT Argument IO a

makeConfig :: SourceSpec -> M TypecheckConfig
makeConfig sourceSpec = do
  Argument {insertTrivial, suppressIfDistribution} <- ask
  pure $
    TypecheckConfig
      { optimizeTrivialAssertion = not insertTrivial,
        distributeIfUnderTensorShape = not suppressIfDistribution,
        sourceSpec = sourceSpec
      }

success :: M (Maybe FailureReason)
success = pure Nothing

failure :: FailureReason -> M (Maybe FailureReason)
failure = pure . Just

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

typecheckStub :: SourceSpec -> [Bind] -> M (Either TypeError (TypeEnv, SigRecord, [AssBind]), TypecheckState)
typecheckStub sourceSpecOfStub bindsInStub = do
  tcConfig <- makeConfig sourceSpecOfStub
  let tcState =
        TypecheckState
          { nextVarIndex = 0,
            assVarDisplay = Map.empty,
            nextTypeVarIndex = 0,
            assTypeVarDisplay = Map.empty,
            implicitArgLogRev = []
          }
      initialTypeEnv = TypeEnv.empty
  pure $
    first (mapLeft fst) $
      Typechecker.run (Typechecker.typecheckBinds () initialTypeEnv bindsInStub) tcConfig tcState

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
  Argument {showParsed} <- ask
  putSectionLine "parsed expression:"
  if showParsed
    then putRenderedLinesAtStage0 e
    else putSkipped "--show-parsed"

displayElaborated :: Map StaticVar Text -> Result0 -> Ass0Expr -> M ()
displayElaborated assVarDisplay result a0e = do
  Argument {showElaborated} <- ask
  putSectionLine "type:"
  if showElaborated
    then putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) result)
    else putSkipped "--show-elaborated"
  putSectionLine "elaborated expression:"
  if showElaborated
    then putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) a0e)
    else putSkipped "--show-elaborated"

displayInferenceResult :: [ImplicitArgLogF Text] -> M ()
displayInferenceResult impArgLogs = do
  Argument {showInferred} <- ask
  putSectionLine $ "inference result (total: " ++ show numTotal ++ ", inferred: " ++ show numInferred ++ "):"
  if showInferred
    then forM_ impArgLogs putRenderedLines
    else putSkipped "--show-inferred"
  where
    numTotal = length impArgLogs
    numInferred = length $ filter (isJust . (^? #_LogInferredArg)) impArgLogs

displayGenerated :: Map StaticVar Text -> Ass1Val -> M ()
displayGenerated assVarDisplay a1v = do
  putSectionLine "generated code:"
  putRenderedLinesAtStage1 (fmap (showVar assVarDisplay) a1v)

typecheckAndEvalInput :: TypecheckState -> SourceSpec -> TypeEnv -> [AssBind] -> Expr -> M (Maybe FailureReason)
typecheckAndEvalInput tcState sourceSpecOfInput tyEnvStub abinds e = do
  Argument {compileTimeOnly} <- ask
  let initialEvalState = Evaluator.initialState sourceSpecOfInput
  (r, TypecheckState {assVarDisplay, implicitArgLogRev}) <-
    typecheckInput sourceSpecOfInput tcState tyEnvStub e
  case r of
    Left tyErr -> do
      putSectionLine "type error:"
      putRenderedLines (fmap (showVar assVarDisplay) tyErr)
      failure ExitByTypeError
    Right (result, a0eWithoutStub) -> do
      let a0e = makeExprFromBinds abinds a0eWithoutStub
      displayElaborated assVarDisplay result a0e
      displayInferenceResult (map (fmap (showVar assVarDisplay)) (reverse implicitArgLogRev))
      case Evaluator.run (Evaluator.evalExpr0 initialEnv a0e) initialEvalState of
        Left err -> do
          putSectionLine "error during compile-time code generation:"
          putRenderedLines (fmap (showVar assVarDisplay) err)
          failure ExitByCompileTimeEvalError
        Right a0v -> do
          case a0v of
            A0ValBracket a1v -> do
              displayGenerated assVarDisplay a1v
              let a0eRuntime = Evaluator.unliftVal a1v
              if compileTimeOnly
                then success
                else case Evaluator.run (Evaluator.evalExpr0 initialEnv a0eRuntime) initialEvalState of
                  Left err -> do
                    putSectionLine "eval error:"
                    putRenderedLines (fmap (showVar assVarDisplay) err)
                    failure ExitByRuntimeEvalError
                  Right a0vRuntime -> do
                    putSectionLine "result of runtime evaluation:"
                    putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) a0vRuntime)
                    success
            _ -> do
              putSectionLine "stage-0 result:"
              putNormalLine "(The stage-0 result was not a code value)"
              putRenderedLinesAtStage0 (fmap (showVar assVarDisplay) a0v)
              if compileTimeOnly
                then success
                else failure ExitByRuntimeEvalError
  where
    initialEnv :: EvalEnv
    initialEnv = Map.empty

typecheckAndEval :: SourceSpec -> [Bind] -> SourceSpec -> Expr -> M (Maybe FailureReason)
typecheckAndEval sourceSpecOfStub bindsInStub sourceSpecOfInput e = do
  (r, tcState@TypecheckState {assVarDisplay}) <- typecheckStub sourceSpecOfStub bindsInStub
  case r of
    Left tyErr -> do
      putSectionLine "type error by stub"
      putRenderedLines (fmap (showVar assVarDisplay) tyErr)
      failure ExitByTypeError
    Right (tyEnvStub, _sigr, abinds) -> do
      typecheckAndEvalInput tcState sourceSpecOfInput tyEnvStub abinds e

handle' :: M (Maybe FailureReason)
handle' = do
  Argument {inputFilePath, stubFilePath} <- ask
  putNormalLine "Staged Shape-Dependent Types (Lambda-Bracket-Assertion)"
  stub_ <- lift $ readFileEither stubFilePath
  case stub_ of
    Left err -> do
      putNormalLine $ "IO error: " ++ err
      failure ExitByIOError
    Right stub -> do
      let sourceSpecOfStub =
            SourceSpec
              { LocationInFile.source = stub,
                LocationInFile.inputFilePath = stubFilePath
              }
      case Parser.parseBinds sourceSpecOfStub stub of
        Left err -> do
          putSectionLine "parse error of stub:"
          putRenderedLines err
          failure ExitByParseError
        Right bindsInStub -> do
          source_ <- lift $ readFileEither inputFilePath
          case source_ of
            Left err -> do
              putNormalLine $ "IO error: " ++ err
              failure ExitByIOError
            Right source -> do
              let sourceSpecOfInput =
                    SourceSpec
                      { LocationInFile.source = source,
                        LocationInFile.inputFilePath = inputFilePath
                      }
              case Parser.parseExpr sourceSpecOfInput source of
                Left err -> do
                  putSectionLine "parse error of source:"
                  putRenderedLines err
                  failure ExitByParseError
                Right e -> do
                  displayParsed e
                  typecheckAndEval sourceSpecOfStub bindsInStub sourceSpecOfInput e

-- Returns a boolean that represents success or failure
handle :: Argument -> IO (Maybe FailureReason)
handle = runReaderT handle'
