module Main where

import Staged.Entrypoint qualified
import Options.Applicative
import Surface.Entrypoint qualified
import System.Exit

defaultDisplayWidth :: Int
defaultDisplayWidth = 120

helpStub, helpDisplayWidth, helpOptimize, helpDistributeIf, helpCompileTimeOnly :: String
helpStub = "Specify the stub file"
helpDisplayWidth = "Set the display width (default: " ++ show defaultDisplayWidth ++ ")"
helpOptimize = "Inserts only non-trivial cast assertions"
helpDistributeIf = "Distributes if-expressions under list literals for tensor shapes"
helpCompileTimeOnly = "Stops after the compile-time evaluation"

data Argument
  = StagedArgument Staged.Entrypoint.Argument
  | SurfaceArgument Surface.Entrypoint.Argument

argumentParser :: Parser Argument
argumentParser =
  subparser
    ( command "lwsd" (info (StagedArgument <$> lwsdArgumentParser <**> helper) (progDesc "Handles staged programs"))
        <> command "surface" (info (SurfaceArgument <$> surfaceArgumentParser <**> helper) (progDesc "Handles non-staged programs"))
    )

lwsdArgumentParser :: Parser Staged.Entrypoint.Argument
lwsdArgumentParser =
  Staged.Entrypoint.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")
    <*> option auto (short 's' <> long "stub" <> value "stub.lwsdi" <> help helpStub)
    <*> switch (short 'O' <> long "optimize" <> help helpOptimize)
    <*> switch (short 'D' <> long "distribute-if" <> help helpDistributeIf)
    <*> option auto (short 'w' <> long "display-width" <> value defaultDisplayWidth <> help helpDisplayWidth)
    <*> switch (short 'c' <> long "compile-time-only" <> help helpCompileTimeOnly)

surfaceArgumentParser :: Parser Surface.Entrypoint.Argument
surfaceArgumentParser =
  Surface.Entrypoint.Argument
    <$> strArgument (metavar "INPUT-FILE-PATH")
    <*> option auto (short 's' <> long "stub" <> value "stub.lwsdi" <> help helpStub)
    <*> switch (short 'O' <> long "optimize" <> help helpOptimize)
    <*> switch (short 'D' <> long "distribute-if" <> help helpDistributeIf)
    <*> option auto (short 'w' <> long "display-width" <> value defaultDisplayWidth <> help helpDisplayWidth)
    <*> switch (short 'c' <> long "compile-time-only" <> help helpCompileTimeOnly)
    <*> switch (short 'd' <> long "default-to-stage-0" <> help "Make ambiguous binding times default to 0, which promotes inlining")

main :: IO ()
main = do
  arg <- execParser (info (argumentParser <**> helper) briefDesc)
  wasSuccess <-
    case arg of
      StagedArgument lwsdArg -> Staged.Entrypoint.handle lwsdArg
      SurfaceArgument surfaceArg -> Surface.Entrypoint.handle surfaceArg
  if wasSuccess
    then exitSuccess
    else exitFailure
