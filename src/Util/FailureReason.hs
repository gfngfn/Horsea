module Util.FailureReason
  ( FailureReason (..),
    makeExitCode,
  )
where

import Prelude

data FailureReason
  = ExitByIOError
  | ExitByParseError
  | ExitByAnalysisError
  | ExitByTypeError
  | ExitByCompileTimeEvalError
  | ExitByRuntimeEvalError

makeExitCode :: FailureReason -> Int
makeExitCode = \case
  ExitByIOError -> 1
  ExitByParseError -> 2
  ExitByAnalysisError -> 3
  ExitByTypeError -> 4
  ExitByCompileTimeEvalError -> 5
  ExitByRuntimeEvalError -> 6
