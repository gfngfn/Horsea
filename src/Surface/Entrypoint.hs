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
import Staged.Typechecker.SigRecord (Ass0Metadata (..), Ass1Metadata (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, ValEntry (..))
import Staged.Typechecker.SigRecord qualified as SigRecord
import Staged.Typechecker (TypecheckState (..))
import Surface.BindingTime qualified as BindingTime
import Surface.BindingTime.Core
import Surface.Parser qualified as Parser
import Util.FailureReason (FailureReason (..))
import Util.IO (readFileEither)
import Util.LocationInFile (SourceSpec (SourceSpec))
import Util.LocationInFile qualified as LocationInFile
import Prelude

data Argument = Argument
  { inputFilePath :: String,
    stubFilePath :: String,
    optimize :: Bool,
    distributeIf :: Bool,
    displayWidth :: Int,
    compileTimeOnly :: Bool,
    fallBackToBindingTime0 :: Bool
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

handle :: Argument -> IO (Maybe FailureReason)
handle Argument {inputFilePath, stubFilePath, optimize, distributeIf, displayWidth, compileTimeOnly, fallBackToBindingTime0} = do
  putStrLn "Lightweight Dependent Types via Staging (Surface Language)"
  let lwArg =
        Staged.Entrypoint.Argument
          { Staged.Entrypoint.inputFilePath = inputFilePath,
            Staged.Entrypoint.stubFilePath = stubFilePath,
            Staged.Entrypoint.optimize = optimize,
            Staged.Entrypoint.distributeIf = distributeIf,
            Staged.Entrypoint.displayWidth = displayWidth,
            Staged.Entrypoint.compileTimeOnly = compileTimeOnly
          }
  stub_ <- readFileEither stubFilePath
  case stub_ of
    Left err -> do
      putStrLn $ "IO error: " ++ err
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
          putRenderedLines err
          failure ExitByParseError
        Right declsInStub -> do
          (r, stateAfterTraversingStub@TypecheckState {assVarDisplay}) <-
            runReaderT (Staged.Entrypoint.typecheckStub sourceSpecOfStub declsInStub) lwArg
          case r of
            Left tyErr -> do
              putSectionLine "type error of stub:"
              putRenderedLines (fmap (Staged.Entrypoint.showVar assVarDisplay) tyErr)
              failure ExitByTypeError
            Right (tyEnvStub, sigr, abinds) -> do
              let initialBindingTimeEnv = makeBindingTimeEnvFromStub sigr
              source_ <- readFileEither inputFilePath
              case source_ of
                Left err -> do
                  putStrLn $ "IO error: " ++ err
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
                      putRenderedLines err
                      failure ExitByParseError
                    Right e -> do
                      putSectionLine "parsed expression:"
                      putRenderedLines e
                      case BindingTime.analyze sourceSpecOfInput fallBackToBindingTime0 initialBindingTimeEnv e of
                        Left analyErr -> do
                          putSectionLine "binding-time analysis error:"
                          putRenderedLines analyErr
                          failure ExitByAnalysisError
                        Right (bce, lwe) -> do
                          putSectionLine "result of binding-time analysis:"
                          putRenderedLines bce
                          putSectionLine "result of staging:"
                          putRenderedLinesAtStage0 lwe
                          runReaderT (Staged.Entrypoint.typecheckAndEvalInput stateAfterTraversingStub sourceSpecOfInput tyEnvStub abinds lwe) lwArg
  where
    putSectionLine :: String -> IO ()
    putSectionLine s = putStrLn ("-------- " ++ s ++ " --------")

    putRenderedLines :: (Disp a) => a -> IO ()
    putRenderedLines = Formatter.putRenderedLines displayWidth

    putRenderedLinesAtStage0 :: (Disp a) => a -> IO ()
    putRenderedLinesAtStage0 = Formatter.putRenderedLinesAtStage0 displayWidth

    failure = return . Just
