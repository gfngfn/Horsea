module Staged.Typechecker.Instantiation
  ( instantiateGuidedByAppContext0,
    instantiateGuidedByAppContext1,
  )
where

import Common.TokenUtil (Span)
import Data.Map qualified as Map
import Data.Set (Set, (\\))
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
  (result, _solution) <- go Set.empty Set.empty Set.empty appCtx0 a0tye0
  pure result
  where
    go :: Set AssVar -> Set AssTypeVar -> Set AssTypeVar -> AppContext -> Ass0TypeExpr -> M trav (Result0, Solution0)
    go varsToInfer tyvars0ToInfer tyvars1ToInfer appCtx a0tye =
      case (appCtx, a0tye) of
        ([], _) ->
          pure (Pure a0tye, Solution0 Map.empty Map.empty Map.empty)
        (AppArg0 labelOpt' a0e1' a0tye1' : appCtx', A0TyArrow labelOpt (xOpt, a0tye1) a0tye2) -> do
          if labelOpt' /= labelOpt
            then do
              spanInFile <- askSpanInFile loc
              typeError trav $ ApplicationLabelMismatch spanInFile appCtx labelOpt' labelOpt
            else do
              (cast, solution1@(Solution0 varSolution1 tyvar0Solution1 tyvar1Solution1)) <-
                makeAssertiveCast trav loc datatyEnv varsToInfer tyvars0ToInfer tyvars1ToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let tyvars1ToInfer' = tyvars1ToInfer \\ Map.keysSet tyvar1Solution1
              let a0tye2s = applySolution0 solution1 a0tye2
              (result', solution') <-
                case xOpt of
                  Nothing -> go varsToInfer' tyvars0ToInfer' tyvars1ToInfer' appCtx' a0tye2s
                  Just x -> go varsToInfer' tyvars0ToInfer' tyvars1ToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let solution = composeSolution0 solution' solution1
              let a0tye1s = applySolution0 solution a0tye1
              let result = Cast0 (fmap (applySolution0 solution') cast) a0tye1s result'
              pure (result, solution)
        (appCtxEntry : appCtx', A0TyOmsArrow label (xOpt, a0tyeElem1) a0tye2) -> do
          case appCtxEntry of
            AppArgOmsGiven0 label' a0e1' a0tyeElem1' | label' == label -> do
              (cast, solution1@(Solution0 varSolution1 tyvar0Solution1 tyvar1Solution1)) <-
                makeAssertiveCast trav loc datatyEnv varsToInfer tyvars0ToInfer tyvars1ToInfer a0tyeElem1' a0tyeElem1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let tyvars1ToInfer' = tyvars1ToInfer \\ Map.keysSet tyvar1Solution1
              let a0tye2s = applySolution0 solution1 a0tye2
              (result', solution') <-
                go varsToInfer' tyvars0ToInfer' tyvars1ToInfer' appCtx' $
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
                go varsToInfer tyvars0ToInfer tyvars1ToInfer appCtx $
                  case xOpt of
                    Nothing -> a0tye2
                    Just x -> subst0 (A0Constructor "Nothing") x a0tye2
              pure (InsertOmitted0 result', solution')
        (appCtxEntry : appCtx', A0TyInfArrow (x, a0tye1) a0tye2) ->
          case appCtxEntry of
            AppArgInfGiven0 a0e1' a0tye1' -> do
              (cast, solution1@(Solution0 varSolution1 tyvar0Solution1 tyvar1Solution1)) <-
                makeAssertiveCast trav loc datatyEnv varsToInfer tyvars0ToInfer tyvars1ToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let tyvars1ToInfer' = tyvars1ToInfer \\ Map.keysSet tyvar1Solution1
              let a0tye2s = applySolution0 solution1 a0tye2
              (result', solution') <-
                go varsToInfer' tyvars0ToInfer' tyvars1ToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let solution = composeSolution0 solution' solution1
              let a0tye1s = applySolution0 solution a0tye1
              let result = CastInfGiven0 (fmap (applySolution0 solution') cast) a0tye1s result'
              pure (result, solution)
            AppArgInfOmitted0 -> do
              (result', solution'@(Solution0 varSolution' tyvar0Solution' tyvar1Solution')) <-
                go (Set.insert x varsToInfer) tyvars0ToInfer tyvars1ToInfer appCtx' a0tye2
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
                  Set.empty
                  Set.empty
                  Set.empty
                  a0tyeInferred
                  (applySolution0 solution' a0tye1)
              let result = FillInferred0 (applyCast0 cast' a0eInferred) result'
              pure (result, Solution0 (Map.delete x varSolution') tyvar0Solution' tyvar1Solution')
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', solution'@(Solution0 varSolution' tyvar0Solution' tyvar1Solution')) <-
                go (Set.insert x varsToInfer) tyvars0ToInfer tyvars1ToInfer appCtx a0tye2
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
                  Set.empty
                  Set.empty
                  Set.empty
                  a0tyeInferred
                  (applySolution0 solution' a0tye1)
              pure (InsertInferred0 (applyCast0 cast' a0eInferred) result', Solution0 (Map.delete x varSolution') tyvar0Solution' tyvar1Solution')
        (_ : _, A0TyCode a1tye) -> do
          (result', varSolution) <- instantiateGuidedByAppContext1 trav loc datatyEnv varsToInfer appCtx a1tye
          let tyvar0Solution = Map.empty
          let tyvar1Solution = Map.empty
          result <- mapMPure (pure . A0TyCode) result'
          pure (result, Solution0 varSolution tyvar0Solution tyvar1Solution)
        (appCtxEntry : appCtx', A0TyForAll fab a0tye2) -> do
          case fab of
            ForAll0 atyvar ->
              case appCtxEntry of
                AppArgInfTypeGiven0 a0tye1' -> do
                  (result', solution') <-
                    go varsToInfer tyvars0ToInfer tyvars1ToInfer appCtx' (tySubst0 a0tye1' atyvar a0tye2)
                  pure (Instantiated0 result', solution')
                _ -> do
                  -- Recurses by using `appCtx`, not `appCtx'`:
                  (result', Solution0 varSolution' tyvar0Solution' tyvar1Solution') <-
                    go varsToInfer (Set.insert atyvar tyvars0ToInfer) tyvars1ToInfer appCtx a0tye2
                  case Map.lookup atyvar tyvar0Solution' of
                    Just a0tyeInferred ->
                      pure (InsertInferredType0 a0tyeInferred result', Solution0 varSolution' (Map.delete atyvar tyvar0Solution') tyvar1Solution')
                    Nothing -> do
                      spanInFile <- askSpanInFile loc
                      typeError trav $ CannotInferTypeVariableInstance0 spanInFile atyvar appCtx a0tye
            ForAll1 _ ->
              error "TODO: instantiateGuidedByAppContext0, ForAll1"
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> DatatypeEnv -> Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, VarSolution)
instantiateGuidedByAppContext1 trav loc datatyEnv varsToInfer0 appCtx0 a1tye0 = do
  (result, Solution1 varSolution _tyvar1Solution) <- go varsToInfer0 Set.empty appCtx0 a1tye0
  pure (result, varSolution)
  where
    go :: Set AssVar -> Set AssTypeVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, Solution1)
    go varsToInfer tyvars1ToInfer appCtx a1tye =
      case (appCtx, a1tye) of
        ([], _) ->
          pure (Pure a1tye, Solution1 Map.empty Map.empty)
        (_ : _, A1TyForAll atyvar a1tye2) -> do
          (result', Solution1 varSolution' tyvar1Solution') <-
            go varsToInfer (Set.insert atyvar tyvars1ToInfer) appCtx a1tye2
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
              (eq, solution1@(Solution1 varSolution1 tyvar1Solution1)) <-
                makeEquation1 trav loc datatyEnv varsToInfer tyvars1ToInfer a1tye1' a1tye1
              (result', solution') <-
                go
                  (varsToInfer \\ Map.keysSet varSolution1)
                  (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
                  appCtx'
                  (applySolution1 solution1 a1tye2)
              let solution = composeSolution1 solution' solution1
              let result = Cast1 (fmap (applySolution1 solution' . A0TyEqAssert loc) eq) a1tye1 result'
              pure (result, solution)
        (appCtxEntry : appCtx', A1TyOmsArrow label a1tye1 a1tye2) ->
          case appCtxEntry of
            AppArgOmsGiven1 label' a1tye1' | label' == label -> do
              (eq, solution1@(Solution1 varSolution1 tyvar1Solution1)) <-
                makeEquation1 trav loc datatyEnv varsToInfer tyvars1ToInfer a1tye1' a1tye1
              (result', solution') <-
                go
                  (varsToInfer \\ Map.keysSet varSolution1)
                  (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
                  appCtx'
                  (applySolution1 solution1 a1tye2)
              let solution = composeSolution1 solution' solution1
              let result = CastOmsGiven1 (fmap (applySolution1 solution' . A0TyEqAssert loc) eq) a1tye1 result'
              pure (result, solution)
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', solution') <- go varsToInfer tyvars1ToInfer appCtx a1tye2
              pure (InsertOmitted1 result', solution')
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye
