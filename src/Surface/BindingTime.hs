module Surface.BindingTime
  ( AnalysisError (..),
    analyze,
  )
where

import Control.Lens
import Data.Generics.Labels ()
import Data.Map qualified as Map
import Staged.SrcSyntax qualified as Staged
import Surface.BindingTime.AnalysisError
import Surface.BindingTime.Analyzer qualified as Analyzer
import Surface.BindingTime.Constraint
import Surface.BindingTime.Core
import Surface.BindingTime.Stager
import Surface.Syntax
import Util.LocationInFile (SourceSpec, getSpanInFile)
import Util.TokenUtil (Span)
import Prelude

type M a = Either AnalysisError a

err :: AnalysisError -> M a
err = Left

analyze :: SourceSpec -> Bool -> BindingTimeEnv -> Expr -> M (BCExprF Span, Staged.Expr)
analyze sourceSpec fallBackToBindingTime0 btenv e = do
  (be', constraints) <- Analyzer.run sourceSpec btenv e
  (rawSolutionMap, _unsolvedConstraints) <-
    case solveConstraints constraints of
      Left ann -> do
        let spanInFile = getSpanInFile sourceSpec ann
        err $ BindingTimeContradiction spanInFile
      Right pair ->
        pure pair
  let solutionMap = Map.mapMaybe (^? #_BTConst) rawSolutionMap
  let btcFallback = if fallBackToBindingTime0 then BT0 else BT1
  let bce =
        fmap
          ( \(bt, ann) ->
              case bt of
                BTConst btc ->
                  (btc, ann)
                BTVar btv ->
                  case Map.lookup btv solutionMap of
                    Just btc -> (btc, ann)
                    Nothing -> (btcFallback, ann)
          )
          be'
  let lwe = stageExpr0 bce
  pure (bce, lwe)
