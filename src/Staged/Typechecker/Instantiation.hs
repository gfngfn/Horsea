module Staged.Typechecker.Instantiation
  ( instantiateGuidedByAppContext0,
    instantiateGuidedByAppContext1,
  )
where

import Common.TokenUtil (Span)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Staged.Subst
import Staged.Syntax
import Staged.TypeError
import Staged.TypeSubst
import Staged.Typechecker.CastInsertion
import Staged.Typechecker.Monad
import Staged.Typechecker.Solution
import Staged.Typechecker.TypeEnv (DatatypeEnv)
import Prelude

instantiateGuidedByAppContext0 :: forall trav. trav -> Span -> DatatypeEnv -> AppContext -> Ass0TypeExpr -> M trav Result0
instantiateGuidedByAppContext0 trav loc datatyEnv appCtx0 a0tye0 = do
  (result, _solution) <- go (SetToInfer0 Set.empty Set.empty Set.empty) appCtx0 a0tye0
  pure result
  where
    go :: SetToInfer0 -> AppContext -> Ass0TypeExpr -> M trav (Result0, Solution0)
    go setToInfer@(SetToInfer0 varsToInfer _ tyvars1ToInfer) appCtx a0tye =
      case (appCtx, a0tye) of
        ([], _) ->
          pure (Pure a0tye, Solution0 Map.empty Map.empty Map.empty)
        (AppArg0 labelOpt' a0e1' a0tye1' : appCtx', A0TyArrow labelOpt (xOpt, a0tye1) a0tye2) -> do
          if labelOpt' /= labelOpt
            then do
              spanInFile <- askSpanInFile loc
              typeError trav $ ApplicationLabelMismatch spanInFile appCtx labelOpt' labelOpt
            else do
              (cast, solution1) <-
                makeAssertiveCast trav loc datatyEnv setToInfer a0tye1' a0tye1
              let setToInfer' = deleteSolutionFromSet0 setToInfer solution1
              let a0tye2s = applySolution0 solution1 a0tye2
              (result', solution') <-
                case xOpt of
                  Nothing -> go setToInfer' appCtx' a0tye2s
                  Just x -> go setToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let solution = composeSolution0 solution' solution1
              let a0tye1s = applySolution0 solution a0tye1
              let result = Cast0 (fmap (applySolution0 solution') cast) a0tye1s result'
              pure (result, solution)
        (appCtxEntry : appCtx', A0TyOmsArrow label (xOpt, a0tyeElem1) a0tye2) -> do
          case appCtxEntry of
            AppArgOmsGiven0 label' a0e1' a0tyeElem1' | label' == label -> do
              (cast, solution1) <-
                makeAssertiveCast trav loc datatyEnv setToInfer a0tyeElem1' a0tyeElem1
              let setToInfer' = deleteSolutionFromSet0 setToInfer solution1
              let a0tye2s = applySolution0 solution1 a0tye2
              (result', solution') <-
                go setToInfer' appCtx' $
                  case xOpt of
                    Nothing -> a0tye2s
                    Just x -> subst0 a0e1' x a0tye2s
              let solution = composeSolution0 solution' solution1
              let a0tyeElem1s = applySolution0 solution a0tyeElem1
              let result = CastOmsGiven0 (fmap (applySolution0 solution') cast) a0tyeElem1s result'
              pure (result, solution)
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', solution') <-
                go setToInfer appCtx $
                  case xOpt of
                    Nothing -> a0tye2
                    Just x -> subst0 (A0Constructor "Nothing") x a0tye2
              pure (InsertOmitted0 result', solution')
        (appCtxEntry : appCtx', A0TyInfArrow (x, a0tye1) a0tye2) ->
          case appCtxEntry of
            AppArgInfGiven0 a0e1' a0tye1' -> do
              (cast, solution1) <-
                makeAssertiveCast trav loc datatyEnv setToInfer a0tye1' a0tye1
              let setToInfer' = deleteSolutionFromSet0 setToInfer solution1
              let a0tye2s = applySolution0 solution1 a0tye2
              (result', solution') <-
                go setToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let solution = composeSolution0 solution' solution1
              let a0tye1s = applySolution0 solution a0tye1
              let result = CastInfGiven0 (fmap (applySolution0 solution') cast) a0tye1s result'
              pure (result, solution)
            AppArgInfOmitted0 -> do
              (result', solution'@(Solution0 varSolution' tyvar0Solution' tyvar1Solution')) <-
                go (addVarToSet0 x setToInfer) appCtx' a0tye2
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x varSolution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferImplicit spanInFile x a0tye appCtx
              (cast', _solution'') <-
                makeAssertiveCast
                  trav
                  loc
                  datatyEnv
                  (SetToInfer0 Set.empty Set.empty Set.empty)
                  a0tyeInferred
                  (applySolution0 solution' a0tye1)
              let result = FillInferred0 (applyCast0 cast' a0eInferred) result'
              pure (result, Solution0 (Map.delete x varSolution') tyvar0Solution' tyvar1Solution')
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', solution'@(Solution0 varSolution' tyvar0Solution' tyvar1Solution')) <-
                go (addVarToSet0 x setToInfer) appCtx a0tye2
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x varSolution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferImplicit spanInFile x a0tye appCtx
              (cast', _solution'') <-
                makeAssertiveCast
                  trav
                  loc
                  datatyEnv
                  (SetToInfer0 Set.empty Set.empty Set.empty)
                  a0tyeInferred
                  (applySolution0 solution' a0tye1)
              pure (InsertInferred0 (applyCast0 cast' a0eInferred) result', Solution0 (Map.delete x varSolution') tyvar0Solution' tyvar1Solution')
        (_ : _, A0TyCode a1tye) -> do
          (result', Solution1 varSolution tyvar1Solution) <-
            instantiateGuidedByAppContext1'
              trav
              loc
              datatyEnv
              (SetToInfer1 varsToInfer tyvars1ToInfer)
              appCtx
              a1tye
          let tyvar0Solution = Map.empty
          result <- mapMPure (pure . A0TyCode) result'
          pure (result, Solution0 varSolution tyvar0Solution tyvar1Solution)
        (appCtxEntry : appCtx', A0TyForAll fab a0tye2) -> do
          case fab of
            ForAll0 atyvar ->
              case appCtxEntry of
                AppArgInfTypeGiven0 a0tye1' -> do
                  (result', solution') <- go setToInfer appCtx' (tySubst0 a0tye1' atyvar a0tye2)
                  pure (Instantiated0 result', solution')
                _ -> do
                  -- Recurses by using `appCtx`, not `appCtx'`:
                  (result', Solution0 varSolution' tyvar0Solution' tyvar1Solution') <-
                    go (addTypeVar0ToSet0 atyvar setToInfer) appCtx a0tye2
                  case Map.lookup atyvar tyvar0Solution' of
                    Just a0tyeInferred ->
                      pure (InsertInferredType0 a0tyeInferred result', Solution0 varSolution' (Map.delete atyvar tyvar0Solution') tyvar1Solution')
                    Nothing -> do
                      spanInFile <- askSpanInFile loc
                      typeError trav $ CannotInferTypeVariableInstance0 spanInFile atyvar appCtx a0tye
            ForAll1 atyvar ->
              case appCtxEntry of
                AppArgInfTypeGiven0 a0tye1' ->
                  case a0tye1' of
                    A0TyCode a1tye1' -> do
                      (result', solution') <- go setToInfer appCtx' (tySubst1 a1tye1' atyvar a0tye2)
                      pure (Instantiated0 result', solution')
                    _ -> do
                      spanInFile <- askSpanInFile loc
                      typeError trav $ NotAStage1TypeVarInstantiation spanInFile a0tye1'
                _ -> do
                  -- Recurses by using `appCtx`, not `appCtx'`:
                  (result', Solution0 varSolution' tyvar0Solution' tyvar1Solution') <-
                    go (addTypeVar1ToSet0 atyvar setToInfer) appCtx a0tye2
                  case Map.lookup atyvar tyvar1Solution' of
                    Just a1tyeInferred ->
                      pure (InsertInferredType0 (A0TyCode a1tyeInferred) result', Solution0 varSolution' tyvar0Solution' (Map.delete atyvar tyvar1Solution'))
                    Nothing -> do
                      spanInFile <- askSpanInFile loc
                      typeError trav $ CannotInferTypeVariableInstance0 spanInFile atyvar appCtx a0tye
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1' :: forall trav. trav -> Span -> DatatypeEnv -> SetToInfer1 -> AppContext -> Ass1TypeExpr -> M trav (Result1, Solution1)
instantiateGuidedByAppContext1' trav loc datatyEnv =
  go
  where
    go :: SetToInfer1 -> AppContext -> Ass1TypeExpr -> M trav (Result1, Solution1)
    go setToInfer@(SetToInfer1 varsToInfer tyvars1ToInfer) appCtx a1tye =
      case (appCtx, a1tye) of
        ([], _) ->
          pure (Pure a1tye, Solution1 Map.empty Map.empty)
        (appCtxEntry : appCtx', A1TyForAll atyvar a1tye2) -> do
          case appCtxEntry of
            AppArgInfTypeGiven1 a1tye1' -> do
              (result', solution') <-
                go setToInfer appCtx' (tySubst1 a1tye1' atyvar a1tye2)
              pure (Instantiated1 result', solution')
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', Solution1 varSolution' tyvar1Solution') <-
                go (SetToInfer1 varsToInfer (Set.insert atyvar tyvars1ToInfer)) appCtx a1tye2
              case Map.lookup atyvar tyvar1Solution' of
                Just a1tyeInferred ->
                  pure (InsertInferredType1 a1tyeInferred result', Solution1 varSolution' (Map.delete atyvar tyvar1Solution'))
                Nothing -> do
                  spanInFile <- askSpanInFile loc
                  typeError trav $ CannotInferTypeVariableInstance1 spanInFile atyvar appCtx a1tye
        (AppArg1 labelOpt' a1tye1' : appCtx', A1TyArrow labelOpt a1tye1 a1tye2) -> do
          if labelOpt' /= labelOpt
            then do
              spanInFile <- askSpanInFile loc
              typeError trav $ ApplicationLabelMismatch spanInFile appCtx labelOpt' labelOpt
            else do
              (eq, solution1) <- makeEquation1 trav loc datatyEnv setToInfer a1tye1' a1tye1
              (result', solution') <-
                go
                  (deleteSolutionFromSet1 setToInfer solution1)
                  appCtx'
                  (applySolution1 solution1 a1tye2)
              let solution = composeSolution1 solution' solution1
              let result = Cast1 (fmap (applySolution1 solution' . A0TyEqAssert loc) eq) a1tye1 result'
              pure (result, solution)
        (appCtxEntry : appCtx', A1TyOmsArrow label a1tye1 a1tye2) ->
          case appCtxEntry of
            AppArgOmsGiven1 label' a1tye1' | label' == label -> do
              (eq, solution1) <- makeEquation1 trav loc datatyEnv setToInfer a1tye1' a1tye1
              (result', solution') <-
                go
                  (deleteSolutionFromSet1 setToInfer solution1)
                  appCtx'
                  (applySolution1 solution1 a1tye2)
              let solution = composeSolution1 solution' solution1
              let result = CastOmsGiven1 (fmap (applySolution1 solution' . A0TyEqAssert loc) eq) a1tye1 result'
              pure (result, solution)
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', solution') <- go setToInfer appCtx a1tye2
              pure (InsertOmitted1 result', solution')
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> DatatypeEnv -> AppContext -> Ass1TypeExpr -> M trav Result1
instantiateGuidedByAppContext1 trav loc datatyEnv appCtx a1tye = do
  (result, _solution) <-
    instantiateGuidedByAppContext1'
      trav
      loc
      datatyEnv
      (SetToInfer1 Set.empty Set.empty)
      appCtx
      a1tye
  pure result
