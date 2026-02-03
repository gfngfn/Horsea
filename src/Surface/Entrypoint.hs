module Surface.Entrypoint
  ( Argument (..),
    handle,
  )
where

import Control.Monad.Trans.Reader
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Staged.Entrypoint qualified
import Staged.Formatter (Disp)
import Staged.Formatter qualified as Formatter
import Staged.Parser qualified as StagedParser
import Staged.SrcSyntax qualified as StagedSyntax
import Staged.Typechecker.Monad (TypecheckState (..))
import Staged.Typechecker.SigRecord (Ass0Metadata (..), Ass1Metadata (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, ValEntry (..))
import Staged.Typechecker.SigRecord qualified as SigRecord
import Surface.BindingTime qualified as BindingTime
import Surface.BindingTime.Core
import Surface.BindingTime.Stager (BCExprF)
import Surface.Parser qualified as Parser
import Surface.Syntax
import Util.FailureReason (FailureReason (..))
import Util.IO (readFileEither)
import Util.LocationInFile (SourceSpec (SourceSpec))
import Util.LocationInFile qualified as LocationInFile
import Util.TokenUtil (Span)
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    stubFilePath :: String,
    insertTrivial :: Bool,
    suppressIfDistribution :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool,
    fallBackToBindingTime0 :: Bool,
    showParsed :: Bool,
    showElaborated :: Bool,
    showInferred :: Bool,
    showBtaResult :: Bool
  }

makeBindingTimeEnvFromStub :: SigRecord -> BindingTimeEnv
makeBindingTimeEnvFromStub =
  SigRecord.fold
    ( \varVal entry bindingTimeEnv ->
        case entry of
          Ass0Entry a0tye a0metadataOpt ->
            let x =
                  -- Uses the same name if not specified
                  case a0metadataOpt of
                    Left Ass0Metadata {ass0surfaceName} -> fromMaybe varVal ass0surfaceName
                    Right _ -> varVal
             in case fromStaged0 a0tye of
                  Nothing ->
                    bindingTimeEnv
                  Just biptyVoid ->
                    Map.insert
                      x
                      (EntryBuiltInFixed0 varVal biptyVoid)
                      bindingTimeEnv
          Ass1Entry a1tye a1metadataOpt ->
            let x =
                  -- Uses the same name if not specified
                  case a1metadataOpt of
                    Left Ass1Metadata {ass1surfaceName} -> fromMaybe varVal ass1surfaceName
                    Right _ -> varVal
                bityVoid = fromStaged1 a1tye
             in Map.insert
                  x
                  (EntryBuiltInFixed1 varVal bityVoid)
                  bindingTimeEnv
          AssPersEntry aPtye AssPersMetadata {assPsurfaceName} ->
            let x =
                  -- Uses the same name if not specified
                  fromMaybe varVal assPsurfaceName
             in case fromStagedPers aPtye of
                  Nothing ->
                    bindingTimeEnv
                  Just bipty ->
                    Map.insert
                      x
                      (EntryBuiltInPersistent varVal bipty)
                      bindingTimeEnv
    )
    ( \varMod (ModuleEntry sigr) bindingTimeEnv ->
        -- Reuses the module name `varMod` in the core language for the surface language.
        Map.insert
          varMod
          (EntryModule (makeBindingTimeEnvFromStub sigr))
          bindingTimeEnv
    )
    Map.empty

putNormalLine :: String -> IO ()
putNormalLine = putStrLn

putSectionLine :: String -> IO ()
putSectionLine s = putStrLn ("-------- " ++ s ++ " --------")

putRenderedLines :: (Disp a) => Argument -> a -> IO ()
putRenderedLines Argument {displayWidth} =
  Formatter.putRenderedLines displayWidth

putRenderedLinesAtStage0 :: (Disp a) => Argument -> a -> IO ()
putRenderedLinesAtStage0 Argument {displayWidth} =
  Formatter.putRenderedLinesAtStage0 displayWidth

putSkipped :: String -> IO ()
putSkipped option =
  putNormalLine $ "  Skipped; specify " ++ option ++ " to see this"

displayParsed :: Argument -> Expr -> IO ()
displayParsed arg@Argument {showParsed} e = do
  putSectionLine "parsed expression:"
  if showParsed
    then putRenderedLines arg e
    else putSkipped "--show-parsed"

displayBtaResult :: Argument -> BCExprF Span -> StagedSyntax.Expr -> IO ()
displayBtaResult arg@Argument {showBtaResult} bce lwe = do
  putSectionLine "result of binding-time analysis:"
  if showBtaResult
    then putRenderedLines arg bce
    else putSkipped "--show-binding-time"
  putSectionLine "result of staging:"
  if showBtaResult
    then putRenderedLinesAtStage0 arg lwe
    else putSkipped "--show-binding-time"

handle :: Argument -> IO (Maybe FailureReason)
handle arg = do
  putNormalLine "Staged Shape-Dependent Types (Horsea)"
  stub_ <- readFileEither stubFilePath
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
      case StagedParser.parseBinds sourceSpecOfStub stub of
        Left err -> do
          putSectionLine "parse error of stub:"
          putRenderedLines arg err
          failure ExitByParseError
        Right declsInStub -> do
          (r, stateAfterTraversingStub@TypecheckState {assVarDisplay}) <-
            runReaderT (Staged.Entrypoint.typecheckStub sourceSpecOfStub declsInStub) lwArg
          case r of
            Left tyErr -> do
              putSectionLine "type error of stub:"
              putRenderedLines arg (fmap (Staged.Entrypoint.showVar assVarDisplay) tyErr)
              failure ExitByTypeError
            Right (tyEnvStub, sigr, abinds) -> do
              let initialBindingTimeEnv = makeBindingTimeEnvFromStub sigr
              source_ <- readFileEither inputFilePath
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
                      putSectionLine "parse error:"
                      putRenderedLines arg err
                      failure ExitByParseError
                    Right e -> do
                      displayParsed arg e
                      case BindingTime.analyze sourceSpecOfInput fallBackToBindingTime0 initialBindingTimeEnv e of
                        Left analyErr -> do
                          putSectionLine "binding-time analysis error:"
                          putRenderedLines arg analyErr
                          failure ExitByAnalysisError
                        Right (bce, lwe) -> do
                          displayBtaResult arg bce lwe
                          runReaderT
                            ( Staged.Entrypoint.typecheckAndEvalInput
                                stateAfterTraversingStub
                                sourceSpecOfInput
                                tyEnvStub
                                abinds
                                lwe
                            )
                            lwArg
  where
    Argument
      { inputFilePath,
        stubFilePath,
        insertTrivial,
        suppressIfDistribution,
        displayWidth,
        compileTimeOnly,
        fallBackToBindingTime0,
        showParsed,
        showElaborated,
        showInferred
      } = arg

    lwArg =
      Staged.Entrypoint.Argument
        { Staged.Entrypoint.inputFilePath = inputFilePath,
          Staged.Entrypoint.stubFilePath = stubFilePath,
          Staged.Entrypoint.insertTrivial = insertTrivial,
          Staged.Entrypoint.suppressIfDistribution = suppressIfDistribution,
          Staged.Entrypoint.displayWidth = displayWidth,
          Staged.Entrypoint.compileTimeOnly = compileTimeOnly,
          Staged.Entrypoint.showParsed = showParsed,
          Staged.Entrypoint.showElaborated = showElaborated,
          Staged.Entrypoint.showInferred = showInferred
        }

    failure = return . Just
