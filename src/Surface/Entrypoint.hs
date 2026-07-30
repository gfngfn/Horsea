module Surface.Entrypoint
  ( Argument (..),
    handle,
  )
where

import Common.FailureReason (FailureReason (..))
import Common.Formatter (Disp)
import Common.Formatter qualified as Formatter
import Common.LocationInFile (SourceSpec (SourceSpec))
import Common.LocationInFile qualified as LocationInFile
import Common.TokenUtil (Span)
import Control.Monad (forM_, unless)
import Control.Monad.Trans.Reader
import Data.Text.IO.Util (readFileEither)
import Staged.Entrypoint qualified
import Staged.Parser qualified as StagedParser
import Staged.SrcSyntax qualified as StagedSyntax
import Staged.Typechecker.Monad (TypecheckState (..))
import Surface.BindingTime qualified as BindingTime
import Surface.BindingTime.FromStaged (makeBindingTimeEnvFromStub)
import Surface.BindingTime.Stager (BCExprF)
import Surface.Parser qualified as Parser
import Surface.Syntax
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
    showBtaResult :: Bool,
    statsOnly :: Bool
  }

putNormalLine :: String -> IO ()
putNormalLine = putStrLn

putSectionLine :: String -> IO ()
putSectionLine s = putStrLn $ "-------- " ++ s ++ " --------"

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
displayParsed arg@Argument {statsOnly, showParsed} e = do
  unless statsOnly $ do
    putSectionLine "parsed expression:"
    if showParsed
      then putRenderedLines arg e
      else putSkipped "--show-parsed"

displayBtaResult :: Argument -> BCExprF Span -> StagedSyntax.Expr -> IO ()
displayBtaResult arg@Argument {statsOnly, showBtaResult} bce lwe = do
  unless statsOnly $ do
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
              let (initialBindingTimeEnv, warnings) = makeBindingTimeEnvFromStub [] sigr
              unless (null warnings) $ do
                putNormalLine "Warnings:"
                forM_ warnings $ \warning ->
                  putRenderedLines arg (fmap (Staged.Entrypoint.showVar assVarDisplay) warning)
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
        showInferred,
        statsOnly
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
          Staged.Entrypoint.showInferred = showInferred,
          Staged.Entrypoint.statsOnly = statsOnly
        }

    failure = return . Just
