module Staged.Typechecker.CastInsertion
  ( applyCast0,
    applyCast1,
    applyEquationCast,
    makeAssertiveCast,
    makeEquation1,
  )
where

import Common.TokenUtil (Span)
import Data.Bifunctor (bimap)
import Data.Functor.Identity
import Data.List.TwoOrMore (TwoOrMore)
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Map qualified as Map
import Data.Maybe1
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Traversable.Compat (mapAccumM)
import Safe.Exact (zipExactMay)
import Staged.BuiltIn qualified as BuiltIn
import Staged.BuiltIn.Core
import Staged.Core
import Staged.Subst
import Staged.Syntax
import Staged.TypeError
import Staged.Typechecker.Monad
import Staged.Typechecker.Solution
import Prelude

applyCast0 :: Maybe Ass0Expr -> Ass0Expr -> Ass0Expr
applyCast0 = maybe id A0App

applyCast1 :: Maybe Ass0Expr -> Ass1Expr -> Ass1Expr
applyCast1 cast a1e =
  case cast of
    Nothing -> a1e
    Just a0eCast -> A1Escape (A0App a0eCast (A0Bracket a1e))

applyEquationCast :: Span -> Maybe Type1Equation -> Ass1Expr -> Ass1Expr
applyEquationCast loc eq =
  applyCast1 (A0TyEqAssert loc <$> eq)

-- | The core part of the cast insertion for stage 0.
-- `makeAssertiveCast trav loc varsToInfer a0tye1 a0tye2` produces a cast
-- that asserts that `a0tye1` is a subtype of `a0tye2`.
-- Returning `(Nothing, ...)` means there's no need to insert a cast.
-- Through cast generation, appropriate expressions for the variables in `varsToInfer`
-- are inferred in a best-effort manner.
makeAssertiveCast :: forall trav. trav -> Span -> Set AssVar -> Set AssTypeVar -> Ass0TypeExpr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr, VarSolution, TypeVar0Solution)
makeAssertiveCast trav loc =
  go
  where
    go :: Set AssVar -> Set AssTypeVar -> Ass0TypeExpr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr, VarSolution, TypeVar0Solution)
    go _varsToInfer _tyvars0ToInfer a0tye1 a0tye2
      | alphaEquivalent a0tye1 a0tye2 =
          pure (Nothing, Map.empty, Map.empty)
    go varsToInfer tyvars0ToInfer a0tye1 a0tye2 = do
      spanInFile <- askSpanInFile loc
      case (a0tye1, a0tye2) of
        (A0TyVar atyvar1, _)
          | atyvar1 `elem` tyvars0ToInfer ->
              pure (Nothing, Map.empty, Map.singleton atyvar1 a0tye2)
        (_, A0TyVar atyvar2)
          | atyvar2 `elem` tyvars0ToInfer ->
              pure (Nothing, Map.empty, Map.singleton atyvar2 a0tye1)
        (A0TyVar atyvar1, A0TyVar atyvar2)
          | atyvar1 == atyvar2 ->
              -- If either `a0tye1` or `a0tye2` is of the form `A0TyVar atyvar`
              -- such that `atyvar` is tracked by the type environment and thereby is not for inference,
              -- then only exact equality is allowed:
              pure (Nothing, Map.empty, Map.empty)
        (A0TyForAll tyvar1 a0tye12, _) -> do
          (cast', varSolution', tyvar0Solution') <-
            go varsToInfer (Set.insert tyvar1 tyvars0ToInfer) a0tye12 a0tye2
          case Map.lookup tyvar1 tyvar0Solution' of
            Just a0tye11 -> do
              cast <- do
                sv <- generateFreshVar Nothing
                let ax = AssVarStatic sv
                pure $
                  Just $
                    A0Lam Nothing (ax, strictify a0tye1) $
                      applyCast0 cast' (A0AppType (A0Var ax) (strictify a0tye11))
              pure (cast, varSolution', tyvar0Solution')
            Nothing ->
              typeError trav $ CannotInstantiateTypeVariableGuidedByAssertion0 spanInFile tyvar1 a0tye12 a0tye2
        (_, A0TyForAll atyvar2 a0tye2') ->
          typeError trav $ Unsupported spanInFile $ HigherRankPolymorphism a0tye1 atyvar2 a0tye2'
        (A0TyPrim a0tyPrim1 maybePred1, A0TyPrim a0tyPrim2 maybePred2') -> do
          -- Ad-hoc optimization of refinement cast insertion.
          -- Maybe we can try using an SMT solver for some subset of predicates here
          -- (if the user allows us to do so) to judge that the LHS predicate implies the RHS one:
          let maybePred2 =
                if alphaEquivalent (Maybe1 maybePred2') (Maybe1 maybePred1)
                  then Nothing
                  else maybePred2'
          cast <-
            if a0tyPrim1 == a0tyPrim2
              then castOrIdentityLam maybePred2 (A0TyPrim a0tyPrim1 maybePred1)
              else typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
          pure (cast, Map.empty, Map.empty)
        (A0TyList a0tye1' maybePred1, A0TyList a0tye2' maybePred2') -> do
          (castForElem, varSolution, tyvar0Solution) <- go varsToInfer tyvars0ToInfer a0tye1' a0tye2'
          -- Ad hoc optimization of refinement cast insertion:
          let maybePred2 =
                if alphaEquivalent (Maybe1 maybePred2') (Maybe1 maybePred1)
                  then Nothing
                  else applySolution0 varSolution tyvar0Solution <$> maybePred2'
          let castForListByElemPred =
                case castForElem of
                  Nothing -> Nothing
                  Just a0eCastForElem -> Just (A0App BuiltIn.ass0exprListMap a0eCastForElem)
          castForListByWholePred <-
            castOrIdentityLam
              maybePred2
              (applySolution0 varSolution tyvar0Solution a0tye1)
          castForList <-
            case (castForListByElemPred, castForListByWholePred) of
              (Nothing, Nothing) ->
                pure Nothing
              (Just a0eCast1, Nothing) ->
                pure $ Just a0eCast1
              (Nothing, Just a0eCast2) ->
                pure $ Just a0eCast2
              (Just a0eCast1, Just a0eCast2) -> do
                sv <- generateFreshVar Nothing
                let ax = AssVarStatic sv
                pure $
                  Just $
                    A0Lam Nothing (ax, strictify a0tye1) $
                      A0App a0eCast2 (A0App a0eCast1 (A0Var ax))
          pure (castForList, varSolution, tyvar0Solution)
        (A0TyMaybe a0tye1', A0TyMaybe a0tye2') -> do
          (castForElem, varSolution, tyvar0Solution) <- go varsToInfer tyvars0ToInfer a0tye1' a0tye2'
          let castForMaybe =
                case castForElem of
                  Nothing -> Nothing
                  Just a0eCastForElem -> Just (A0App BuiltIn.ass0exprMaybeMap a0eCastForElem)
          pure (castForMaybe, varSolution, tyvar0Solution)
        (A0TyProduct a0tyes1, A0TyProduct a0tyes2) -> do
          zipped <-
            case TwoOrMore.zipExact a0tyes1 a0tyes2 of
              Just zipped' -> pure zipped'
              Nothing -> typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
          ((_, _, varSolutionRet, tyvar0SolutionRet), castAndDomTypePairs') <-
            mapAccumM
              ( \(varsToInfer', tyvars0ToInfer', varSolution', tyvar0Solution') (a0tye1', a0tye2') -> do
                  (cast, varSolution, tyvar0Solution) <-
                    go
                      varsToInfer'
                      tyvars0ToInfer'
                      (applySolution0 varSolution' tyvar0Solution' a0tye1')
                      (applySolution0 varSolution' tyvar0Solution' a0tye2')
                  pure
                    ( ( varsToInfer' \\ Map.keysSet varSolution,
                        tyvars0ToInfer' \\ Map.keysSet tyvar0Solution,
                        composeVarSolution varSolution varSolution',
                        composeTypeVar0Solution tyvar0Solution tyvar0Solution'
                      ),
                      (cast, a0tye1')
                    )
              )
              (varsToInfer, tyvars0ToInfer, Map.empty, Map.empty)
              zipped
          let castAndDomTypePairs =
                fmap
                  ( bimap
                      (fmap (applySolution0 varSolutionRet tyvar0SolutionRet))
                      (applySolution0 varSolutionRet tyvar0SolutionRet)
                  )
                  castAndDomTypePairs'
          cast <- makeProductTypeCast trav castAndDomTypePairs
          pure (cast, varSolutionRet, tyvar0SolutionRet)
        (A0TyArrow labelOpt1 (x1opt, a0tye11) a0tye12, A0TyArrow labelOpt2 (x2opt, a0tye21) a0tye22withX2opt) -> do
          if labelOpt1 /= labelOpt2
            then
              typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
            else do
              (castDom, varSolutionDom, tyvar0SolutionDom) <- go varsToInfer tyvars0ToInfer a0tye11 a0tye21
              (x, a0tye22) <-
                case (x1opt, x2opt) of
                  (Nothing, Nothing) -> do
                    sv <- generateFreshVar Nothing
                    let x0 = AssVarStatic sv
                    pure (x0, a0tye22withX2opt)
                  (Just x1, Nothing) ->
                    pure (x1, a0tye22withX2opt)
                  (Nothing, Just x2) ->
                    pure (x2, a0tye22withX2opt)
                  (Just x1, Just x2) ->
                    pure (x1, subst0 (A0Var x1) x2 a0tye22withX2opt)
              (castCod, varSolutionCod, tyvar0SolutionCod) <-
                go
                  (varsToInfer \\ Map.keysSet varSolutionDom)
                  (tyvars0ToInfer \\ Map.keysSet tyvar0SolutionDom)
                  (applySolution0 varSolutionDom tyvar0SolutionDom a0tye12)
                  (applySolution0 varSolutionDom tyvar0SolutionDom a0tye22)
              let varSolution = composeVarSolution varSolutionCod varSolutionDom
              let tyvar0Solution = composeTypeVar0Solution tyvar0SolutionDom tyvar0SolutionCod
              cast <-
                makeArrowTypeCast
                  trav
                  x
                  (applySolution0 varSolution tyvar0Solution a0tye11)
                  (applySolution0 varSolution tyvar0Solution a0tye12)
                  (applySolution0 varSolution tyvar0Solution a0tye21)
                  (applySolution0 varSolutionCod tyvar0Solution <$> castDom)
                  castCod
              pure (cast, varSolution, tyvar0Solution)
        (A0TyOmsArrow label1 (x1opt, a0tye11) a0tye12, A0TyOmsArrow label2 (x2opt, a0tye21) a0tye22withX2opt) -> do
          if label1 /= label2
            then
              typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2
            else do
              (castDom, varSolutionDom, tyvar0SolutionDom) <- go varsToInfer tyvars0ToInfer a0tye11 a0tye21
              (x, a0tye22) <-
                case (x1opt, x2opt) of
                  (Nothing, Nothing) -> do
                    sv <- generateFreshVar Nothing
                    let x0 = AssVarStatic sv
                    pure (x0, a0tye22withX2opt)
                  (Just x1, Nothing) ->
                    pure (x1, a0tye22withX2opt)
                  (Nothing, Just x2) ->
                    pure (x2, a0tye22withX2opt)
                  (Just x1, Just x2) ->
                    pure (x1, subst0 (A0Var x1) x2 a0tye22withX2opt)
              (castCod, varSolutionCod, tyvar0SolutionCod) <-
                go
                  (varsToInfer \\ Map.keysSet varSolutionDom)
                  (tyvars0ToInfer \\ Map.keysSet tyvar0SolutionDom)
                  (applySolution0 varSolutionDom tyvar0SolutionDom a0tye12)
                  (applySolution0 varSolutionDom tyvar0SolutionDom a0tye22)
              let varSolution = composeVarSolution varSolutionCod varSolutionDom
              let tyvar0Solution = composeTypeVar0Solution tyvar0SolutionCod tyvar0SolutionDom
              cast <-
                makeArrowTypeCast
                  trav
                  x
                  (A0TyMaybe (applySolution0 varSolution tyvar0Solution a0tye11))
                  (applySolution0 varSolution tyvar0Solution a0tye12)
                  (A0TyMaybe (applySolution0 varSolution tyvar0Solution a0tye21))
                  (applySolution0 varSolutionCod tyvar0Solution <$> castDom)
                  castCod
              pure (cast, varSolution, tyvar0Solution)
        (A0TyInfArrow (x1, a0tye11) a0tye12, A0TyInfArrow (x2, a0tye21) a0tye22withX2) -> do
          (castDom, varSolutionDom, tyvar0SolutionDom) <- go varsToInfer tyvars0ToInfer a0tye11 a0tye21
          let (x, a0tye22) = (x1, subst0 (A0Var x1) x2 a0tye22withX2)
          (castCod, varSolutionCod, tyvar0SolutionCod) <-
            go
              (varsToInfer \\ Map.keysSet varSolutionDom)
              (tyvars0ToInfer \\ Map.keysSet tyvar0SolutionDom)
              (applySolution0 varSolutionDom tyvar0SolutionDom a0tye12)
              (applySolution0 varSolutionDom tyvar0SolutionDom a0tye22)
          let varSolution = composeVarSolution varSolutionCod varSolutionDom
          let tyvar0Solution = composeTypeVar0Solution tyvar0SolutionCod tyvar0SolutionDom
          -- We can use the same cast function as `A0TyArrow`:
          cast <-
            makeArrowTypeCast
              trav
              x
              (applySolution0 varSolution tyvar0Solution a0tye11)
              (applySolution0 varSolution tyvar0Solution a0tye12)
              (applySolution0 varSolution tyvar0Solution a0tye21)
              (applySolution0 varSolutionCod tyvar0SolutionCod <$> castDom)
              castCod
          pure (cast, varSolution, tyvar0Solution)
        (A0TyCode a1tye1, A0TyCode a1tye2) -> do
          (eq, varSolution, _tyvar1Solution) <- makeEquation1 trav loc varsToInfer Set.empty a1tye1 a1tye2
          let tyvar0Solution = Map.empty
          pure (A0TyEqAssert loc <$> eq, varSolution, tyvar0Solution)
        (_, _) ->
          typeError trav $ TypeContradictionAtStage0 spanInFile a0tye1 a0tye2

    makeArrowTypeCast :: trav -> AssVar -> Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr -> Maybe Ass0Expr -> Maybe Ass0Expr -> M trav (Maybe Ass0Expr)
    makeArrowTypeCast _trav x a0tye11 a0tye12 a0tye21 castDom castCod =
      case (castDom, castCod) of
        (Nothing, Nothing) ->
          pure Nothing
        (_, _) -> do
          f <- AssVarStatic <$> generateFreshVar Nothing
          x' <- AssVarStatic <$> generateFreshVar Nothing
          let fDom = applyCast0 castDom
          let fCod = applyCast0 castCod
          let sa0tye1 = SA0TyArrow (Just x, strictify a0tye11) (strictify a0tye12)
          pure $
            Just $
              A0Lam Nothing (f, sa0tye1) $
                A0Lam Nothing (x, strictify a0tye21) $
                  A0App (A0Lam Nothing (x', strictify a0tye11) (fCod (A0App (A0Var f) (A0Var x')))) (fDom (A0Var x))

    makeProductTypeCast :: trav -> TwoOrMore (Maybe Ass0Expr, Ass0TypeExpr) -> M trav (Maybe Ass0Expr)
    makeProductTypeCast _trav castAndDomTypePairs =
      if all ((== Nothing) . fst) castAndDomTypePairs
        then
          pure Nothing
        else do
          x <- AssVarStatic <$> generateFreshVar Nothing
          let a0tyeAnnot = A0TyProduct (fmap snd castAndDomTypePairs)
          let n = length castAndDomTypePairs
          pure . Just $
            A0Lam Nothing (x, strictify a0tyeAnnot) $
              A0Tuple $
                TwoOrMore.mapIndexed
                  ( \i (cast, _a0tye) ->
                      applyCast0
                        cast
                        (A0App (A0BuiltInName (BuiltInArity1 (BIProj n i))) (A0Var x))
                  )
                  castAndDomTypePairs

    castOrIdentityLam :: Maybe Ass0Expr -> Ass0TypeExpr -> M trav (Maybe Ass0Expr)
    castOrIdentityLam maybePred2 a0tye1 = do
      TypecheckConfig {optimizeTrivialAssertion} <- askConfig
      case maybePred2 of
        Nothing ->
          if optimizeTrivialAssertion
            then pure Nothing
            else Just <$> makeIdentityLam a0tye1
        Just a0ePred2 -> do
          x <- AssVarStatic <$> generateFreshVar Nothing
          pure $ Just (A0Lam Nothing (x, strictify a0tye1) (A0RefinementAssert loc a0ePred2 (A0Var x)))

-- | The core part of the cast insertion for stage 1.
makeEquation1 :: forall trav. trav -> Span -> Set AssVar -> Set AssTypeVar -> Ass1TypeExpr -> Ass1TypeExpr -> M trav (Maybe Type1Equation, VarSolution, TypeVar1Solution)
makeEquation1 trav loc varsToInferInit tyvars1ToInferInit a1tye1Whole a1tye2Whole = do
  TypecheckConfig {optimizeTrivialAssertion} <- askConfig
  spanInFile <- askSpanInFile loc
  case go varsToInferInit tyvars1ToInferInit a1tye1Whole a1tye2Whole of
    Right (trivial, ty1eq, varSolution, tyvar1Solution) ->
      if trivial && optimizeTrivialAssertion
        then pure (Nothing, varSolution, tyvar1Solution)
        else pure (Just ty1eq, varSolution, tyvar1Solution)
    Left () ->
      typeError trav $ TypeContradictionAtStage1 spanInFile a1tye1Whole a1tye2Whole
  where
    checkExprArgs :: Set AssVar -> (Ass0Expr, Ass0TypeExpr) -> Ass0Expr -> (Bool, Ass0Expr, VarSolution)
    checkExprArgs varsToInfer (a0e1, a0tye1) a0e2 =
      case a0e2 of
        A0Var x | x `elem` varsToInfer -> (True, a0e1, Map.singleton x (a0e1, a0tye1))
        _ -> (alphaEquivalent a0e1 a0e2, a0e2, Map.empty)

    go :: Set AssVar -> Set AssTypeVar -> Ass1TypeExpr -> Ass1TypeExpr -> Either () (Bool, Type1Equation, VarSolution, TypeVar1Solution)
    go varsToInfer tyvars1ToInfer a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyVar atyvar1, _)
          | atyvar1 `elem` tyvars1ToInfer ->
              pure (True, makeTrivialEquationFromType1 a1tye2, Map.empty, Map.singleton atyvar1 a1tye2)
        (_, A1TyVar atyvar2)
          | atyvar2 `elem` tyvars1ToInfer ->
              pure (True, makeTrivialEquationFromType1 a1tye1, Map.empty, Map.singleton atyvar2 a1tye1)
        (A1TyVar atyvar1, A1TyVar atyvar2)
          | atyvar1 == atyvar2 ->
              -- If either `a0tye1` or `a0tye2` is of the form `A0TyVar atyvar`
              -- such that `atyvar` is tracked by the type environment and thereby is not for inference,
              -- then only exact equality is allowed:
              pure (True, makeTrivialEquationFromType1 a1tye1, Map.empty, Map.empty)
        (A1TyPrim a1tyPrim1, A1TyPrim a1tyPrim2) ->
          case (a1tyPrim1, a1tyPrim2) of
            (A1TyPrimBase tyPrimBase1, A1TyPrimBase tyPrimBase2) ->
              if tyPrimBase1 == tyPrimBase2
                then pure (True, TyEq1Prim (TyEq1PrimBase tyPrimBase1), Map.empty, Map.empty)
                else Left ()
            (A1TyTensor a0eList1, A1TyTensor a0eList2) -> do
              (trivial, listEq, varSolution) <- goList varsToInfer a0eList1 a0eList2
              pure (trivial, TyEq1Prim (TyEq1Tensor listEq), varSolution, Map.empty)
            (A1TyDataset dp1, A1TyDataset dp2) -> do
              let (trivialOnNumTrain, numTrain2', varSolutionByNumTrain) =
                    checkExprArgs
                      varsToInfer
                      (dp1.numTrain, BuiltIn.tyNat)
                      dp2.numTrain
              let solAcc1 = varSolutionByNumTrain
              let varsToInferAcc1 = varsToInfer \\ Map.keysSet varSolutionByNumTrain

              let (trivialOnNumTest, numTest2', varSolutionByNumTest) =
                    checkExprArgs
                      varsToInferAcc1
                      (applyVarSolution varSolutionByNumTrain dp1.numTest, BuiltIn.tyNat)
                      dp2.numTest
              let solAcc2 = composeVarSolution solAcc1 varSolutionByNumTest
              let varsToInferAcc2 = varsToInferAcc1 \\ Map.keysSet varSolutionByNumTest

              (trivialOnImage, listEqOfImage, varSolutionByImage) <-
                goList
                  varsToInferAcc2
                  (applyVarSolution solAcc2 (runIdentity dp1.image))
                  (applyVarSolution solAcc2 (runIdentity dp2.image))
              let solAcc3 = composeVarSolution solAcc2 varSolutionByImage
              let varsToInferAcc3 = varsToInferAcc2 \\ Map.keysSet varSolutionByImage

              (trivialOnLabel, listEqOfLabel, varSolutionByLabel) <-
                goList
                  varsToInferAcc3
                  (applyVarSolution solAcc3 (runIdentity dp1.label))
                  (applyVarSolution solAcc3 (runIdentity dp2.label))
              let solAcc4 = composeVarSolution solAcc3 varSolutionByLabel

              let finalize :: forall af. (HasVar StaticVar af) => af StaticVar -> af StaticVar
                  finalize = applyVarSolution solAcc4

              let datasetParamEq =
                    DatasetParamEquation
                      { numTrainEq = (finalize dp1.numTrain, finalize numTrain2'),
                        numTestEq = (finalize dp1.numTest, finalize numTest2'),
                        imageEq = finalize listEqOfImage,
                        labelEq = listEqOfLabel
                      }
              pure
                ( trivialOnNumTrain && trivialOnNumTest && trivialOnImage && trivialOnLabel,
                  TyEq1Prim (TyEq1Dataset datasetParamEq),
                  solAcc4,
                  Map.empty
                )
            (A1TyLstm a0eInputSize1 a0eHiddenSize1, A1TyLstm a0eInputSize2 a0eHiddenSize2) -> do
              let (trivialOnInputSize, inputSize2', varSolutionByInputSize) =
                    checkExprArgs
                      varsToInfer
                      (a0eInputSize1, BuiltIn.tyNat)
                      a0eInputSize2
              let solAcc1 = varSolutionByInputSize
              let varsToInferAcc1 = varsToInfer \\ Map.keysSet varSolutionByInputSize

              let (trivialOnHiddenSize, hiddenSize2', varSolutionByHiddenSize) =
                    checkExprArgs
                      varsToInferAcc1
                      (applyVarSolution varSolutionByInputSize a0eHiddenSize1, BuiltIn.tyNat)
                      a0eHiddenSize2
              let solAcc2 = composeVarSolution solAcc1 varSolutionByHiddenSize

              let finalize :: forall af. (HasVar StaticVar af) => af StaticVar -> af StaticVar
                  finalize = applyVarSolution solAcc2

              pure
                ( trivialOnInputSize && trivialOnHiddenSize,
                  TyEq1Prim
                    ( TyEq1Lstm
                        (finalize a0eInputSize1, finalize inputSize2')
                        (finalize a0eHiddenSize1, finalize hiddenSize2')
                    ),
                  solAcc2,
                  Map.empty
                )
            (A1TyTextHelper a0e1, A1TyTextHelper a0e2) -> do
              let (trivial, a0e2', varSolution) = checkExprArgs varsToInfer (a0e1, BuiltIn.tyNat) a0e2
              pure (trivial, TyEq1Prim (TyEq1TextHelper (a0e1, a0e2')), varSolution, Map.empty)
            (_, _) ->
              Left ()
        (A1TyList a1tye1elem, A1TyList a1tye2elem) -> do
          (trivial, ty1eqElem, varSolution, tyvar1Solution) <- go varsToInfer tyvars1ToInfer a1tye1elem a1tye2elem
          pure (trivial, TyEq1List ty1eqElem, varSolution, tyvar1Solution)
        (A1TyMaybe a1tye1elem, A1TyMaybe a1tye2elem) -> do
          (trivial, ty1eqElem, varSolution, tyvar1Solution) <- go varsToInfer tyvars1ToInfer a1tye1elem a1tye2elem
          pure (trivial, TyEq1Maybe ty1eqElem, varSolution, tyvar1Solution)
        (A1TyProduct a1tyes1, A1TyProduct a1tyes2) -> do
          zipped <-
            case TwoOrMore.zipExact a1tyes1 a1tyes2 of
              Just zipped' -> pure zipped'
              Nothing -> Left ()
          ((_, _, trivialRet, varSolutionRet, tyvar1SolutionRet), ty1eqs) <-
            mapAccumM
              ( \(varsToInfer', tyvars1ToInfer', trivial', varSolution', tyvar1Solution') (a1tye1', a1tye2') -> do
                  (trivial, ty1eq, varSolution, tyvar1Solution) <-
                    go
                      varsToInfer'
                      tyvars1ToInfer'
                      (applySolution1 varSolution' tyvar1Solution' a1tye1')
                      (applySolution1 varSolution' tyvar1Solution' a1tye2')
                  pure
                    ( ( varsToInfer' \\ Map.keysSet varSolution,
                        tyvars1ToInfer' \\ Map.keysSet tyvar1Solution,
                        trivial' && trivial,
                        composeVarSolution varSolution' varSolution,
                        composeTypeVar1Solution tyvar1Solution' tyvar1Solution
                      ),
                      ty1eq
                    )
              )
              (varsToInfer, tyvars1ToInfer, True, Map.empty, Map.empty)
              zipped
          let ty1eqsRet = applySolution1 varSolutionRet tyvar1SolutionRet <$> ty1eqs
          pure (trivialRet, TyEq1Product ty1eqsRet, varSolutionRet, tyvar1SolutionRet)
        (A1TyRecord ra1ty1, A1TyRecord ra1ty2) -> do
          rpair <-
            if Map.keysSet ra1ty1 == Map.keysSet ra1ty2
              then pure $ Map.intersectionWith (,) ra1ty1 ra1ty2
              else Left ()
          ((_, _, trivialRet, varSolutionRet, tyvar1SolutionRet), rty1eq) <-
            mapAccumM
              ( \(varsToInfer', tyvars1ToInfer', trivial', varSolution', tyvar1Solution') (a1tye1', a1tye2') -> do
                  (trivial, ty1eq, varSolution, tyvar1Solution) <-
                    go
                      varsToInfer'
                      tyvars1ToInfer'
                      (applySolution1 varSolution' tyvar1Solution' a1tye1')
                      (applySolution1 varSolution' tyvar1Solution' a1tye2')
                  pure
                    ( ( varsToInfer' \\ Map.keysSet varSolution,
                        tyvars1ToInfer' \\ Map.keysSet tyvar1Solution,
                        trivial' && trivial,
                        composeVarSolution varSolution' varSolution,
                        composeTypeVar1Solution tyvar1Solution' tyvar1Solution
                      ),
                      ty1eq
                    )
              )
              (varsToInfer, tyvars1ToInfer, True, Map.empty, Map.empty)
              rpair
          let rty1eqRet = applySolution1 varSolutionRet tyvar1SolutionRet <$> rty1eq
          pure (trivialRet, TyEq1Record rty1eqRet, varSolutionRet, tyvar1SolutionRet)
        (A1TyArrow labelOpt1 a1tye11 a1tye12, A1TyArrow labelOpt2 a1tye21 a1tye22) -> do
          if labelOpt1 /= labelOpt2
            then
              Left ()
            else do
              (trivial1, ty1eqDom, varSolution1, tyvar1Solution1) <- go varsToInfer tyvars1ToInfer a1tye11 a1tye21
              (trivial2, ty1eqCod, varSolution2, tyvar1Solution2) <-
                go
                  (varsToInfer \\ Map.keysSet varSolution1)
                  (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
                  a1tye12
                  (applyVarSolution varSolution1 a1tye22)
              let varSolution = composeVarSolution varSolution1 varSolution2
              let tyvar1Solution = composeTypeVar1Solution tyvar1Solution1 tyvar1Solution2
              pure (trivial1 && trivial2, TyEq1Arrow labelOpt1 ty1eqDom ty1eqCod, varSolution, tyvar1Solution)
        (A1TyOmsArrow label1 a1tye11 a1tye12, A1TyOmsArrow label2 a1tye21 a1tye22) -> do
          if label1 /= label2
            then
              Left ()
            else do
              (trivial1, ty1eqDom, varSolution1, tyvar1Solution1) <- go varsToInfer tyvars1ToInfer a1tye11 a1tye21
              (trivial2, ty1eqCod, varSolution2, tyvar1Solution2) <-
                go
                  (varsToInfer \\ Map.keysSet varSolution1)
                  (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
                  a1tye12
                  (applyVarSolution varSolution1 a1tye22)
              let varSolution = composeVarSolution varSolution1 varSolution2
              let tyvar1Solution = composeTypeVar1Solution tyvar1Solution1 tyvar1Solution2
              pure (trivial1 && trivial2, TyEq1OmsArrow label1 ty1eqDom ty1eqCod, varSolution, tyvar1Solution)
        (_, A1TyForAll atyvar2 a1tye22) ->
          -- Not confident. TODO (theory): ensure that this works correctly
          go varsToInfer (Set.insert atyvar2 tyvars1ToInfer) a1tye1 a1tye22
        (_, _) ->
          Left ()

    goList :: Set AssVar -> Ass0Expr -> Ass0Expr -> Either () (Bool, ListEquation, VarSolution)
    goList varsToInfer a0eList1 a0eList2 =
      case (a0eList1, a0eList2) of
        -- Enhancement for the argument inference 1:
        (A0Literal (ALitList a0es1), A0Literal (ALitList a0es2)) ->
          case zipExactMay a0es1 a0es2 of
            Nothing ->
              Left ()
            Just zipped -> do
              let (trivial, equationAccResult, _varsToInfer, varSolution) =
                    foldl'
                      ( \(trivialAcc, equationAcc, varsToInferAcc, varSolutionAcc) (a0e1, a0e2) ->
                          let a0e1sub = applyVarSolution varSolutionAcc a0e1
                              a0e2sub = applyVarSolution varSolutionAcc a0e2
                              (trivial', a0e2', varSolution') =
                                checkExprArgs varsToInferAcc (a0e1sub, BuiltIn.tyNat) a0e2sub
                           in ( trivialAcc && trivial',
                                (a0e1sub, a0e2') : equationAcc,
                                varsToInferAcc \\ Map.keysSet varSolution',
                                composeVarSolution varSolution' varSolutionAcc
                              )
                      )
                      (True, [], varsToInfer, Map.empty)
                      zipped
              let listEq = ListEqByElements (reverse equationAccResult)
              pure (trivial, listEq, varSolution)
        -- Enhancement for the argument inference 2:
        (_, A0Var x2)
          | x2 `elem` varsToInfer -> do
              let listEq = ListEqByWhole a0eList1 a0eList1
              let varSolution = Map.singleton x2 (a0eList1, A0TyList BuiltIn.tyNat Nothing)
              pure (True, listEq, varSolution)
        -- General rule:
        (_, _) -> do
          let trivial = alphaEquivalent a0eList1 a0eList2
          let listEq = ListEqByWhole a0eList1 a0eList2
          pure (trivial, listEq, Map.empty)
