module Main where

import Options.Applicative
import Staged.Entrypoint qualified
import Surface.Entrypoint qualified
import System.Exit
import Util.FailureReason (makeExitCode)
import Prelude

defaultDisplayWidth :: Int
defaultDisplayWidth = 120

helpStub, helpDisplayWidth, helpInsertTrivial, helpSuppressIfDistribution, helpCompileTimeOnly, helpDefaultToStage0, helpShowParsed, helpShowBindingTime, helpShowElaborated, helpShowInferred :: String
helpStub = "Specify the stub file"
helpDisplayWidth = "Set the display width (default: " ++ show defaultDisplayWidth ++ ")"
helpInsertTrivial = "Inserts trivial cast assertions as well as non-trivial ones"
helpSuppressIfDistribution = "Suppress the distribution of if-expressions under list literals for tensor shapes"
helpCompileTimeOnly = "Stops after the compile-time evaluation"
helpDefaultToStage0 = "Make ambiguous binding times default to 0, which promotes inlining"
helpShowParsed = "Display the parsed expression"
helpShowBindingTime = "Display the result of binding-time analysis"
helpShowElaborated = "Display the elaborated expression"
helpShowInferred = "Display the inferred arguments"

data Argument
  = StagedArgument Staged.Entrypoint.Argument
  | SurfaceArgument Surface.Entrypoint.Argument

argumentParser :: Parser Argument
argumentParser =
  subparser
    ( command "staged" (info (StagedArgument <$> stagedArgumentParser <**> helper) (progDesc "Handles staged programs"))
        <> command "surface" (info (SurfaceArgument <$> surfaceArgumentParser <**> helper) (progDesc "Handles non-staged programs"))
    )

stagedArgumentParser :: Parser Staged.Entrypoint.Argument
stagedArgumentParser =
  Staged.Entrypoint.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")
    <*> option auto (short 's' <> long "stub" <> value "stub.lbam" <> help helpStub)
    <*> switch (long "insert-trivial" <> help helpInsertTrivial)
    <*> switch (long "suppress-if-distribution" <> help helpSuppressIfDistribution)
    <*> option auto (short 'w' <> long "display-width" <> value defaultDisplayWidth <> help helpDisplayWidth)
    <*> switch (short 'c' <> long "compile-time-only" <> help helpCompileTimeOnly)
    <*> switch (long "show-parsed" <> help helpShowParsed)
    <*> switch (long "show-elaborated" <> help helpShowElaborated)
    <*> switch (long "show-inferred" <> help helpShowInferred)

surfaceArgumentParser :: Parser Surface.Entrypoint.Argument
surfaceArgumentParser =
  Surface.Entrypoint.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")
    <*> option auto (short 's' <> long "stub" <> value "stub.lbam" <> help helpStub)
    <*> switch (long "insert-trivial" <> help helpInsertTrivial)
    <*> switch (long "suppress-if-distribution" <> help helpSuppressIfDistribution)
    <*> option auto (short 'w' <> long "display-width" <> value defaultDisplayWidth <> help helpDisplayWidth)
    <*> switch (short 'c' <> long "compile-time-only" <> help helpCompileTimeOnly)
    <*> switch (short 'd' <> long "default-to-stage-0" <> help helpDefaultToStage0)
    <*> switch (long "show-parsed" <> help helpShowParsed)
    <*> switch (long "show-elaborated" <> help helpShowElaborated)
    <*> switch (long "show-inferred" <> help helpShowInferred)
    <*> switch (long "show-binding-time" <> help helpShowBindingTime)

main :: IO ()
main = do
  arg <- execParser (info (argumentParser <**> helper) briefDesc)
  failureReasonOpt <-
    case arg of
      StagedArgument lwsdArg -> Staged.Entrypoint.handle lwsdArg
      SurfaceArgument surfaceArg -> Surface.Entrypoint.handle surfaceArg
  case failureReasonOpt of
    Nothing -> exitSuccess
    Just failureReason -> exitWith (ExitFailure (makeExitCode failureReason))
