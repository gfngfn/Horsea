module Staged.Typechecker
  ( typecheckExpr0,
    typecheckExpr1,
    typecheckTypeExpr0,
    typecheckTypeExpr1,
    typecheckBind,
    typecheckBinds,
    run,
  )
where

import Control.Monad
import Data.Either.Extra
import Data.Foldable (foldrM)
import Data.Function
import Data.Functor.Identity
import Data.List qualified as List
import Data.List.Extra qualified as List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Tuple.Extra
import Safe.Exact
import Staged.BuiltIn qualified as BuiltIn
import Staged.BuiltIn.Core
import Staged.Core
import Staged.SrcSyntax
import Staged.Subst
import Staged.Syntax
import Staged.TypeError
import Staged.TypeSubst
import Staged.Typechecker.Monad
import Staged.Typechecker.SigRecord (Ass0Metadata (..), Ass1Metadata (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, ValEntry (..))
import Staged.Typechecker.SigRecord qualified as SigRecord
import Staged.Typechecker.TypeEnv (TypeEnv, TypeVarEntry (..))
import Staged.Typechecker.TypeEnv qualified as TypeEnv
import Util.LocationInFile (SpanInFile, getSpanInFile)
import Util.Matrix qualified as Matrix
import Util.Maybe1
import Util.TokenUtil (Span)
import Util.Vector qualified as Vector
import Prelude

bug :: String -> a
bug msg = error $ "bug: " ++ msg

askSpanInFile :: Span -> M trav SpanInFile
askSpanInFile loc = do
  TypecheckConfig {sourceSpec} <- askConfig
  pure $ getSpanInFile sourceSpec loc

findValVar :: trav -> Span -> [Var] -> Var -> TypeEnv -> M trav ValEntry
findValVar trav loc ms x tyEnv = do
  spanInFile <- askSpanInFile loc
  liftEither $ maybeToEither (UnboundVar spanInFile ms x, trav) $ do
    case ms of
      [] ->
        TypeEnv.findVal x tyEnv
      m : ms' -> do
        ModuleEntry sigr <- TypeEnv.findModule m tyEnv
        go sigr ms'
  where
    go :: SigRecord -> [Var] -> Maybe ValEntry
    go sigr [] =
      SigRecord.findVal x sigr
    go sigr (m : ms') = do
      ModuleEntry sigr' <- SigRecord.findModule m sigr
      go sigr' ms'

findTypeVar :: trav -> Span -> TypeVar -> TypeEnv -> M trav TypeVarEntry
findTypeVar trav loc tyvar tyEnv = do
  spanInFile <- askSpanInFile loc
  case TypeEnv.findTypeVar tyvar tyEnv of
    Nothing ->
      typeError trav $ UnboundTypeVar spanInFile tyvar
    Just tyVarEntry ->
      pure tyVarEntry

makeIdentityLam :: Ass0TypeExpr -> M trav Ass0Expr
makeIdentityLam a0tye = do
  sv <- generateFreshVar Nothing
  let ax = AssVarStatic sv
  pure $ A0Lam Nothing (ax, strictify a0tye) (A0Var ax)

applyCast :: Maybe Ass0Expr -> Ass0Expr -> Ass0Expr
applyCast = maybe id A0App

applyCast1 :: Maybe Ass0Expr -> Ass1Expr -> Ass1Expr
applyCast1 cast a1e =
  case cast of
    Nothing -> a1e
    Just a0eCast -> A1Escape (A0App a0eCast (A0Bracket a1e))

applyEquationCast :: Span -> Maybe Type1Equation -> Ass1Expr -> Ass1Expr
applyEquationCast loc eq =
  applyCast1 (A0TyEqAssert loc <$> eq)

-- The core part of the cast insertion for stage 0.
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
        (A0TyImplicitForAll tyvar1 a0tye12, _) -> do
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
                      applyCast cast' (A0AppType (A0Var ax) (strictify a0tye11))
              pure (cast, varSolution', tyvar0Solution')
            Nothing ->
              typeError trav $ CannotInstantiateTypeVariableGuidedByAssertion0 spanInFile tyvar1 a0tye12 a0tye2
        (_, A0TyImplicitForAll atyvar2 a0tye2') ->
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
                  else maybePred2'
          let castForListByElemPred =
                case castForElem of
                  Nothing -> Nothing
                  Just a0eCastForElem -> Just (A0App ass0exprListMap a0eCastForElem)
          castForListByWholePred <- castOrIdentityLam maybePred2 a0tye1
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
                      A0App (A0App ass0exprAnd (A0App a0eCast1 (A0Var ax))) (A0App a0eCast2 (A0Var ax))
          pure (castForList, varSolution, tyvar0Solution)
        (A0TyProduct a0tye11 a0tye12, A0TyProduct a0tye21 a0tye22) -> do
          (cast1, varSolution1, tyvar0Solution1) <- go varsToInfer tyvars0ToInfer a0tye11 a0tye21
          (cast2, varSolution2, tyvar0Solution2) <-
            go
              (varsToInfer \\ Map.keysSet varSolution1)
              (tyvars0ToInfer \\ Map.keysSet tyvar0Solution1)
              (applySolution0 varSolution1 tyvar0Solution1 a0tye12)
              (applySolution0 varSolution1 tyvar0Solution1 a0tye22)
          let varSolution = composeVarSolution varSolution1 varSolution2
          let tyvar0Solution = composeTypeVar0Solution tyvar0Solution1 tyvar0Solution2
          cast <-
            makeProductTypeCast
              trav
              (applySolution0 varSolution tyvar0Solution a0tye11)
              (applySolution0 varSolution tyvar0Solution a0tye12)
              (applySolution0 varSolution2 tyvar0Solution <$> cast1)
              cast2
          pure (cast, varSolution, tyvar0Solution)
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
              let varSolution = composeVarSolution varSolutionDom varSolutionCod
              let tyvar0Solution = composeTypeVar0Solution tyvar0SolutionDom tyvar0SolutionCod
              cast <-
                makeFunctionTypeCast
                  trav
                  x
                  (applySolution0 varSolution tyvar0Solution a0tye11)
                  (applySolution0 varSolution tyvar0Solution a0tye12)
                  (applySolution0 varSolution tyvar0Solution a0tye21)
                  (applySolution0 varSolutionCod tyvar0Solution <$> castDom)
                  castCod
              pure (cast, varSolution, tyvar0Solution)
        (A0TyImpArrow (x1, a0tye11) a0tye12, A0TyImpArrow (x2, a0tye21) a0tye22withX2) -> do
          (castDom, varSolutionDom, tyvar0SolutionDom) <- go varsToInfer tyvars0ToInfer a0tye11 a0tye21
          let (x, a0tye22) = (x1, subst0 (A0Var x1) x2 a0tye22withX2)
          (castCod, varSolutionCod, tyvar0SolutionCod) <-
            go
              (varsToInfer \\ Map.keysSet varSolutionDom)
              (tyvars0ToInfer \\ Map.keysSet tyvar0SolutionDom)
              (applySolution0 varSolutionDom tyvar0SolutionDom a0tye12)
              (applySolution0 varSolutionDom tyvar0SolutionDom a0tye22)
          let varSolution = composeVarSolution varSolutionDom varSolutionCod
          let tyvar0Solution = composeTypeVar0Solution tyvar0SolutionDom tyvar0SolutionCod
          cast <-
            makeFunctionTypeCast
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

    makeFunctionTypeCast :: trav -> AssVar -> Ass0TypeExpr -> Ass0TypeExpr -> Ass0TypeExpr -> Maybe Ass0Expr -> Maybe Ass0Expr -> M trav (Maybe Ass0Expr)
    makeFunctionTypeCast _trav x a0tye11 a0tye12 a0tye21 castDom castCod =
      case (castDom, castCod) of
        (Nothing, Nothing) ->
          pure Nothing
        (_, _) -> do
          let a0tye1 = A0TyArrow Nothing (Just x, a0tye11) a0tye12
          f <- AssVarStatic <$> generateFreshVar Nothing
          x' <- AssVarStatic <$> generateFreshVar Nothing
          let fDom = applyCast castDom
          let fCod = applyCast castCod
          pure $
            Just $
              A0Lam Nothing (f, strictify a0tye1) $
                A0Lam Nothing (x, strictify a0tye21) $
                  A0App (A0Lam Nothing (x', strictify a0tye11) (fCod (A0App (A0Var f) (A0Var x')))) (fDom (A0Var x))

    makeProductTypeCast :: trav -> Ass0TypeExpr -> Ass0TypeExpr -> Maybe Ass0Expr -> Maybe Ass0Expr -> M trav (Maybe Ass0Expr)
    makeProductTypeCast _trav a0tye11 a0tye12 cast1 cast2 =
      case (cast1, cast2) of
        (Nothing, Nothing) ->
          pure Nothing
        (_, _) -> do
          x <- AssVarStatic <$> generateFreshVar Nothing
          let f1 = applyCast cast1
          let f2 = applyCast cast2
          pure $
            Just $
              A0Lam Nothing (x, strictify (A0TyProduct a0tye11 a0tye12)) $
                A0Tuple
                  (f1 (A0App (A0BuiltInName (BuiltInArity1 BIFst)) (A0Var x)))
                  (f2 (A0App (A0BuiltInName (BuiltInArity1 BISnd)) (A0Var x)))

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

-- The core part of the cast insertion for stage 1.
makeEquation1 :: forall trav. trav -> Span -> Set AssVar -> Set AssTypeVar -> Ass1TypeExpr -> Ass1TypeExpr -> M trav (Maybe Type1Equation, VarSolution, TypeVar1Solution)
makeEquation1 trav loc varsToInfer' tyvars1ToInfer' a1tye1' a1tye2' = do
  TypecheckConfig {optimizeTrivialAssertion} <- askConfig
  spanInFile <- askSpanInFile loc
  case go varsToInfer' tyvars1ToInfer' a1tye1' a1tye2' of
    Right (trivial, ty1eq, varSolution, tyvar1Solution) ->
      if trivial && optimizeTrivialAssertion
        then pure (Nothing, varSolution, tyvar1Solution)
        else pure (Just ty1eq, varSolution, tyvar1Solution)
    Left () ->
      typeError trav $ TypeContradictionAtStage1 spanInFile a1tye1' a1tye2'
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
        (A1TyProduct a1tye11 a1tye12, A1TyProduct a1tye21 a1tye22) -> do
          (trivial1, ty1eq1, varSolution1, tyvar1Solution1) <- go varsToInfer tyvars1ToInfer a1tye11 a1tye21
          (trivial2, ty1eq2, varSolution2, tyvar1Solution2) <-
            go
              (varsToInfer \\ Map.keysSet varSolution1)
              (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
              a1tye12
              (applyVarSolution varSolution1 a1tye22)
          let varSolution = composeVarSolution varSolution1 varSolution2
          let tyvar1Solution = composeTypeVar1Solution tyvar1Solution1 tyvar1Solution2
          pure (trivial1 && trivial2, TyEq1Product ty1eq1 ty1eq2, varSolution, tyvar1Solution)
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
        (_, A1TyImplicitForAll atyvar2 a1tye22) ->
          -- Not confident. TODO: ensure that this works correctly
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
                    List.foldl'
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

mergeTypesByConditional0 :: forall trav. trav -> Bool -> Ass0Expr -> Ass0TypeExpr -> Ass0TypeExpr -> M' ConditionalMergeError trav Ass0TypeExpr
mergeTypesByConditional0 trav distributeIfUnderTensorShape a0e0 = go0
  where
    go0 :: Ass0TypeExpr -> Ass0TypeExpr -> M' ConditionalMergeError trav Ass0TypeExpr
    go0 a0tye1 a0tye2 =
      case (a0tye1, a0tye2) of
        (A0TyPrim a0tyePrim1 maybePred1, A0TyPrim a0tyePrim2 maybePred2) ->
          if a0tyePrim1 == a0tyePrim2
            then do
              maybePred <- mergeRefinementPredicates (SA0TyPrim a0tyePrim1) maybePred1 maybePred2
              pure $ A0TyPrim a0tyePrim1 maybePred
            else
              typeError trav $ CannotMerge0 a0tye1 a0tye2
        (A0TyList a0tye1' maybePred1, A0TyList a0tye2' maybePred2) -> do
          a0tye <- go0 a0tye1' a0tye2'
          maybePred <- mergeRefinementPredicates (SA0TyList (strictify a0tye1')) maybePred1 maybePred2
          pure $ A0TyList a0tye maybePred
        (A0TyArrow labelOpt1 (x1opt, a0tye11) a0tye12, A0TyArrow labelOpt2 (x2opt, a0tye21) a0tye22) -> do
          if labelOpt1 /= labelOpt2
            then
              typeError trav $ CannotMerge0 a0tye1 a0tye2
            else do
              a0tye1u <- go0 a0tye11 a0tye21
              (xu, a0tye2u) <-
                case (x1opt, x2opt) of
                  (Nothing, Nothing) -> (Nothing,) <$> go0 a0tye12 a0tye22
                  (Just x1, Nothing) -> (Just x1,) <$> go0 a0tye12 a0tye22
                  (Nothing, Just x2) -> (Just x2,) <$> go0 a0tye12 a0tye22
                  (Just x1, Just x2) -> (Just x1,) <$> go0 a0tye12 (subst0 (A0Var x1) x2 a0tye22)
              pure $ A0TyArrow labelOpt1 (xu, a0tye1u) a0tye2u
        (A0TyCode a1tye1, A0TyCode a1tye2) ->
          A0TyCode <$> go1 a1tye1 a1tye2
        (A0TyImpArrow (x1, a0tye11) a0tye12, A0TyImpArrow (x2, a0tye21) a0tye22) -> do
          a0tye1u <- go0 a0tye11 a0tye21
          a0tye2u <- go0 a0tye12 (subst0 (A0Var x1) x2 a0tye22)
          pure $ A0TyImpArrow (x1, a0tye1u) a0tye2u
        _ ->
          typeError trav $ CannotMerge0 a0tye1 a0tye2

    mergeRefinementPredicates :: (Maybe Ass0Expr -> StrictAss0TypeExpr) -> Maybe Ass0Expr -> Maybe Ass0Expr -> M' ConditionalMergeError trav (Maybe Ass0Expr)
    mergeRefinementPredicates sa0tyef maybePred1 maybePred2 =
      case (maybePred1, maybePred2) of
        (Nothing, Nothing) -> pure Nothing
        (Just a0ePred1, Nothing) -> makeIf (A0App a0ePred1 . A0Var) (\_ -> A0Literal (ALitBool True))
        (Nothing, Just a0ePred2) -> makeIf (\_ -> A0Literal (ALitBool True)) (A0App a0ePred2 . A0Var)
        (Just a0ePred1, Just a0ePred2) -> makeIf (A0App a0ePred1 . A0Var) (A0App a0ePred2 . A0Var)
      where
        makeIf f1 f2 = do
          ax <- AssVarStatic <$> generateFreshVar Nothing
          pure $ Just (A0Lam Nothing (ax, sa0tyef Nothing) (A0IfThenElse a0e0 (f1 ax) (f2 ax)))

    go1 :: Ass1TypeExpr -> Ass1TypeExpr -> M' ConditionalMergeError trav Ass1TypeExpr
    go1 = mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0

mergeTypesByConditional1 :: forall trav. trav -> Bool -> Ass0Expr -> Ass1TypeExpr -> Ass1TypeExpr -> M' ConditionalMergeError trav Ass1TypeExpr
mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0 = go1
  where
    go1 :: Ass1TypeExpr -> Ass1TypeExpr -> M' ConditionalMergeError trav Ass1TypeExpr
    go1 a1tye1 a1tye2 =
      case (a1tye1, a1tye2) of
        (A1TyPrim a1tyePrim1, A1TyPrim a1tyePrim2) ->
          A1TyPrim
            <$> case (a1tyePrim1, a1tyePrim2) of
              (A1TyPrimBase tyPrimBase1, A1TyPrimBase tyPrimBase2) ->
                if tyPrimBase1 == tyPrimBase2
                  then pure a1tyePrim1
                  else typeError trav $ CannotMerge1 a1tye1 a1tye2
              (A1TyTensor a0eList1, A1TyTensor a0eList2) ->
                case (a0eList1, a0eList2) of
                  -- Slight enhancement for the argument inference:
                  (A0Literal (ALitList a0es1), A0Literal (ALitList a0es2)) | distributeIfUnderTensorShape ->
                    case zipExactMay a0es1 a0es2 of
                      Nothing -> typeError trav $ CannotMerge1 a1tye1 a1tye2
                      Just zipped -> pure $ A1TyTensor (A0Literal (ALitList (map (uncurry a0branch) zipped)))
                  -- General rule:
                  (_, _) ->
                    pure $ A1TyTensor (a0branch a0eList1 a0eList2)
              (A1TyDataset dp1, A1TyDataset dp2) ->
                pure . A1TyDataset $
                  DatasetParam
                    { numTrain = a0branch dp1.numTrain dp2.numTrain,
                      numTest = a0branch dp1.numTest dp2.numTest,
                      image = Identity (a0branch (runIdentity dp1.image) (runIdentity dp2.image)),
                      label = Identity (a0branch (runIdentity dp1.label) (runIdentity dp2.label))
                    }
              (A1TyLstm a0eInputSize1 a0eHiddenSize1, A1TyLstm a0eInputSize2 a0eHiddenSize2) ->
                pure $ A1TyLstm (a0branch a0eInputSize1 a0eInputSize2) (a0branch a0eHiddenSize1 a0eHiddenSize2)
              (A1TyTextHelper a0eLabels1, A1TyTextHelper a0eLabels2) ->
                pure $ A1TyTextHelper (a0branch a0eLabels1 a0eLabels2)
              _ ->
                typeError trav $ CannotMerge1 a1tye1 a1tye2
        (A1TyArrow labelOpt1 a1tye11 a1tye12, A1TyArrow labelOpt2 a1tye21 a1tye22) ->
          if labelOpt1 == labelOpt2
            then A1TyArrow labelOpt1 <$> go1 a1tye11 a1tye21 <*> go1 a1tye12 a1tye22
            else typeError trav $ CannotMerge1 a1tye1 a1tye2
        _ ->
          typeError trav $ CannotMerge1 a1tye1 a1tye2

    a0branch = A0IfThenElse a0e0

mergeResultsByConditional0 :: forall trav. trav -> Span -> Ass0Expr -> Result0 -> Result0 -> M trav Result0
mergeResultsByConditional0 trav loc a0e0 = go
  where
    go result1 result2 =
      case (result1, result2) of
        (Pure a0tye1, Pure a0tye2) ->
          Pure <$> mergeTypes0 a0tye1 a0tye2
        (Cast0 cast1 a0tye1 r1, Cast0 cast2 a0tye2 r2) -> do
          a0tye <- mergeTypes0 a0tye1 a0tye2
          cast <- mergeCasts cast1 a0tye1 cast2 a0tye2
          Cast0 cast a0tye <$> go r1 r2
        (Cast1 cast1 a1tye1 r1, Cast1 cast2 a1tye2 r2) -> do
          a1tye <- mergeTypes1 a1tye1 a1tye2
          cast <- mergeCasts cast1 (A0TyCode a1tye1) cast2 (A0TyCode a1tye2)
          Cast1 cast a1tye <$> go r1 r2
        (CastGiven0 cast1 a0tye1 r1, CastGiven0 cast2 a0tye2 r2) -> do
          a0tye <- mergeTypes0 a0tye1 a0tye2
          cast <- mergeCasts cast1 a0tye1 cast2 a0tye2
          CastGiven0 cast a0tye <$> go r1 r2
        (FillInferred0 a0e1 r1, FillInferred0 a0e2 r2) -> do
          FillInferred0 (a0branch a0e1 a0e2) <$> go r1 r2
        (InsertInferred0 a0e1 r1, InsertInferred0 a0e2 r2) -> do
          InsertInferred0 (a0branch a0e1 a0e2) <$> go r1 r2
        _ -> do
          -- Reachable if two branches of an if-expression are inconsistent as to `InsertInferred0`.
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotMergeResultsByConditionals spanInFile result1 result2

    a0branch = A0IfThenElse a0e0

    mergeTypes0 :: Ass0TypeExpr -> Ass0TypeExpr -> M trav Ass0TypeExpr
    mergeTypes0 a0tye1 a0tye2 = do
      TypecheckConfig {distributeIfUnderTensorShape} <- askConfig
      spanInFile <- askSpanInFile loc
      mapTypeError (CannotMergeTypesByConditional0 spanInFile a0tye1 a0tye2) $
        mergeTypesByConditional0 trav distributeIfUnderTensorShape a0e0 a0tye1 a0tye2

    mergeTypes1 :: Ass1TypeExpr -> Ass1TypeExpr -> M trav Ass1TypeExpr
    mergeTypes1 a1tye1 a1tye2 = do
      TypecheckConfig {distributeIfUnderTensorShape} <- askConfig
      spanInFile <- askSpanInFile loc
      mapTypeError (CannotMergeTypesByConditional1 spanInFile a1tye1 a1tye2) $
        mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0 a1tye1 a1tye2

    mergeCasts cast1 a0tye1 cast2 a0tye2 =
      case (cast1, cast2) of
        (Nothing, Nothing) ->
          pure Nothing
        (Just a0e1, Nothing) -> do
          a0e2 <- makeIdentityLam a0tye2
          pure $ Just (a0branch a0e1 a0e2)
        (Nothing, Just a0e2) -> do
          a0e1 <- makeIdentityLam a0tye1
          pure $ Just (a0branch a0e1 a0e2)
        (Just a0e1, Just a0e2) -> do
          pure $ Just (a0branch a0e1 a0e2)

type VarSolution = Map AssVar (Ass0Expr, Ass0TypeExpr)

type TypeVar0Solution = Map AssTypeVar Ass0TypeExpr

type TypeVar1Solution = Map AssTypeVar Ass1TypeExpr

applyVarSolution :: forall af. (HasVar StaticVar af) => VarSolution -> af StaticVar -> af StaticVar
applyVarSolution varSolution entity =
  Map.foldrWithKey (flip subst0) entity (Map.map fst varSolution)

applyTypeVar0Solution :: forall af. (HasTypeVar af) => TypeVar0Solution -> af StaticVar -> af StaticVar
applyTypeVar0Solution tyvar0Solution entity =
  Map.foldrWithKey (flip tySubst0) entity tyvar0Solution

applyTypeVar1Solution :: forall af. (HasTypeVar af) => TypeVar1Solution -> af StaticVar -> af StaticVar
applyTypeVar1Solution tyvar1Solution entity =
  Map.foldrWithKey (flip tySubst1) entity tyvar1Solution

composeVarSolution :: VarSolution -> VarSolution -> VarSolution
composeVarSolution solNew solOld =
  Map.union
    solNew
    (Map.map (\(a0e, a0tye) -> (applyVarSolution solNew a0e, applyVarSolution solNew a0tye)) solOld)

composeTypeVar0Solution :: TypeVar0Solution -> TypeVar0Solution -> TypeVar0Solution
composeTypeVar0Solution solNew solOld =
  Map.union solNew (Map.map (applyTypeVar0Solution solNew) solOld)

composeTypeVar1Solution :: TypeVar1Solution -> TypeVar1Solution -> TypeVar1Solution
composeTypeVar1Solution solNew solOld =
  Map.union solNew (Map.map (applyTypeVar1Solution solNew) solOld)

applySolution0 :: forall af. (HasVar StaticVar af, HasTypeVar af) => VarSolution -> TypeVar0Solution -> af StaticVar -> af StaticVar
applySolution0 varSolution tyvar0Solution entity =
  applyTypeVar0Solution tyvar0Solution (applyVarSolution varSolution entity)

applySolution1 :: forall af. (HasVar StaticVar af, HasTypeVar af) => VarSolution -> TypeVar1Solution -> af StaticVar -> af StaticVar
applySolution1 varSolution tyvar1Solution entity =
  applyTypeVar1Solution tyvar1Solution (applyVarSolution varSolution entity)

instantiateGuidedByAppContext0 :: forall trav. trav -> Span -> AppContext -> Ass0TypeExpr -> M trav Result0
instantiateGuidedByAppContext0 trav loc appCtx0 a0tye0 = do
  (result, _varSolution, _tyvar0Solution) <- go Set.empty Set.empty appCtx0 a0tye0
  pure result
  where
    go :: Set AssVar -> Set AssTypeVar -> AppContext -> Ass0TypeExpr -> M trav (Result0, VarSolution, TypeVar0Solution)
    go varsToInfer tyvars0ToInfer appCtx a0tye =
      case (appCtx, a0tye) of
        ([], _) ->
          pure (Pure a0tye, Map.empty, Map.empty)
        (AppArg0 labelOpt' a0e1' a0tye1' : appCtx', A0TyArrow labelOpt (xOpt, a0tye1) a0tye2) -> do
          if labelOpt' /= labelOpt
            then do
              spanInFile <- askSpanInFile loc
              typeError trav $ ApplicationLabelMismatch spanInFile appCtx labelOpt' labelOpt
            else do
              (cast, varSolution1, tyvar0Solution1) <-
                makeAssertiveCast trav loc varsToInfer tyvars0ToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let a0tye2s = applySolution0 varSolution1 tyvar0Solution1 a0tye2
              (result', varSolution', tyvar0Solution') <-
                case xOpt of
                  Nothing -> go varsToInfer' tyvars0ToInfer' appCtx' a0tye2s
                  Just x -> go varsToInfer' tyvars0ToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let varSolution = composeVarSolution varSolution1 varSolution'
              let tyvar0Solution = composeTypeVar0Solution tyvar0Solution1 tyvar0Solution'
              let a0tye1s = applySolution0 varSolution tyvar0Solution a0tye1
              let result = Cast0 (fmap (applySolution0 varSolution' tyvar0Solution') cast) a0tye1s result'
              pure (result, varSolution, tyvar0Solution)
        (appCtxEntry : appCtx', A0TyImpArrow (x, a0tye1) a0tye2) ->
          case appCtxEntry of
            AppArgImpGiven0 a0e1' a0tye1' -> do
              (cast, varSolution1, tyvar0Solution1) <-
                makeAssertiveCast trav loc varsToInfer tyvars0ToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let a0tye2s = applySolution0 varSolution1 tyvar0Solution1 a0tye2
              (result', varSolution', tyvar0Solution') <-
                go varsToInfer' tyvars0ToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let varSolution = composeVarSolution varSolution1 varSolution'
              let tyvar0Solution = composeTypeVar0Solution tyvar0Solution1 tyvar0Solution'
              let a0tye1s = applySolution0 varSolution tyvar0Solution a0tye1
              let result = CastGiven0 (fmap (applySolution0 varSolution' tyvar0Solution') cast) a0tye1s result'
              pure (result, varSolution, tyvar0Solution)
            AppArgImpOmitted0 -> do
              (result', varSolution', tyvar0Solution') <-
                go (Set.insert x varsToInfer) tyvars0ToInfer appCtx' a0tye2
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x varSolution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferImplicit spanInFile x a0tye appCtx
              (cast', _varSolution'', _tyvar0Solution'') <-
                makeAssertiveCast
                  trav
                  loc
                  Set.empty
                  Set.empty
                  a0tyeInferred
                  (applySolution0 varSolution' tyvar0Solution' a0tye1)
              let result = FillInferred0 (applyCast cast' a0eInferred) result'
              pure (result, varSolution', tyvar0Solution')
            _ -> do
              (result', varSolution', tyvar0Solution') <-
                go (Set.insert x varsToInfer) tyvars0ToInfer appCtx a0tye2
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x varSolution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferImplicit spanInFile x a0tye appCtx
              (cast', _varSolution'', _tyvar0Solution'') <-
                makeAssertiveCast
                  trav
                  loc
                  Set.empty
                  Set.empty
                  a0tyeInferred
                  (applySolution0 varSolution' tyvar0Solution' a0tye1)
              pure (InsertInferred0 (applyCast cast' a0eInferred) result', varSolution', tyvar0Solution')
        (_ : _, A0TyCode a1tye) -> do
          (result', varSolution) <- instantiateGuidedByAppContext1 trav loc varsToInfer appCtx a1tye
          let tyvar0Solution = Map.empty
          result <- mapMPure (pure . A0TyCode) result'
          pure (result, varSolution, tyvar0Solution)
        (_ : _, A0TyImplicitForAll atyvar a0tye2) -> do
          (result', varSolution', tyvar0Solution') <-
            go varsToInfer (Set.insert atyvar tyvars0ToInfer) appCtx a0tye2
          case Map.lookup atyvar tyvar0Solution' of
            Just a0tyeInferred ->
              pure (InsertInferredType0 a0tyeInferred result', varSolution', tyvar0Solution')
            Nothing -> do
              spanInFile <- askSpanInFile loc
              typeError trav $ CannotInferTypeVariableInstance0 spanInFile atyvar appCtx a0tye
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, VarSolution)
instantiateGuidedByAppContext1 trav loc varsToInfer0 appCtx0 a1tye0 = do
  (result, varSolution, _tyvar1Solution) <- go varsToInfer0 Set.empty appCtx0 a1tye0
  pure (result, varSolution)
  where
    go :: Set AssVar -> Set AssTypeVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, VarSolution, TypeVar1Solution)
    go varsToInfer tyvars1ToInfer appCtx a1tye =
      case (appCtx, a1tye) of
        ([], _) ->
          pure (Pure a1tye, Map.empty, Map.empty)
        (_ : _, A1TyImplicitForAll atyvar a1tye2) -> do
          (result', varSolution', tyvar1Solution') <-
            go varsToInfer (Set.insert atyvar tyvars1ToInfer) appCtx a1tye2
          case Map.lookup atyvar tyvar1Solution' of
            Just a1tyeInferred ->
              pure (InsertType1 a1tyeInferred result', varSolution', tyvar1Solution')
            Nothing -> do
              spanInFile <- askSpanInFile loc
              typeError trav $ CannotInferTypeVariableInstance1 spanInFile atyvar appCtx a1tye
        (AppArg1 labelOpt' a1tye1' : appCtx', A1TyArrow labelOpt a1tye1 a1tye2) -> do
          if labelOpt' /= labelOpt
            then do
              spanInFile <- askSpanInFile loc
              typeError trav $ ApplicationLabelMismatch spanInFile appCtx labelOpt' labelOpt
            else do
              (eq, varSolution1, tyvar1Solution1) <-
                makeEquation1 trav loc varsToInfer tyvars1ToInfer a1tye1' a1tye1
              (result', varSolution', tyvar1Solution') <-
                go
                  (varsToInfer \\ Map.keysSet varSolution1)
                  (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
                  appCtx'
                  (applySolution1 varSolution1 tyvar1Solution1 a1tye2)
              let varSolution = composeVarSolution varSolution1 varSolution'
              let tyvar1Solution = composeTypeVar1Solution tyvar1Solution1 tyvar1Solution'
              let result = Cast1 (fmap (applySolution1 varSolution' tyvar1Solution' . A0TyEqAssert loc) eq) a1tye1 result'
              pure (result, varSolution, tyvar1Solution)
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye

forceExpr0 :: trav -> TypeEnv -> Ass0TypeExpr -> Expr -> M trav Ass0Expr
forceExpr0 trav tyEnv a0tyeReq e@(Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  case eMain of
    Literal (LitList es) ->
      case a0tyeReq of
        A0TyList a0tyeElem _maybePred -> do
          a0es <- mapM (forceExpr0 trav tyEnv a0tyeElem) es
          pure $ A0Literal (ALitList a0es)
        _ ->
          typeError trav $ CannotForceType spanInFile a0tyeReq
    _ -> do
      (a0tye, a0e) <- typecheckExpr0Single trav tyEnv e
      (cast, _varSolution, _tyvar0Solution) <- makeAssertiveCast trav loc Set.empty Set.empty a0tye a0tyeReq
      pure $ applyCast cast a0e

typecheckExpr0Single :: trav -> TypeEnv -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckExpr0Single trav tyEnv e@(Expr loc _) = do
  (result0, a0e) <- typecheckExpr0 trav tyEnv [] e
  case result0 of
    Pure a0tye ->
      pure (a0tye, a0e)
    _ -> do
      spanInFile <- askSpanInFile loc
      bug $ "non-empty result0: " ++ show spanInFile

typecheckExpr0 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result0, Ass0Expr)
typecheckExpr0 trav tyEnv appCtx (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  completeInferredImplicit spanInFile
    =<< case eMain of
      Literal lit ->
        case appCtx of
          [] -> do
            (a0tye, alit) <-
              case lit of
                LitInt n ->
                  -- Ad hoc optimization about `Nat`
                  pure (if n >= 0 then BuiltIn.tyNat else A0TyPrim (A0TyPrimBase ATyPrimInt) Nothing, ALitInt n)
                LitFloat r ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimFloat) Nothing, ALitFloat r)
                LitUnit ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimUnit) Nothing, ALitUnit)
                LitBool b ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimBool) Nothing, ALitBool b)
                LitString t ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimString) Nothing, ALitString t)
                LitList es ->
                  case es of
                    [] ->
                      typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
                    eFirst : esTail -> do
                      (a0tyeFirst, a0eFirst) <- typecheckExpr0Single trav tyEnv eFirst
                      a0esTail <- mapM (forceExpr0 trav tyEnv a0tyeFirst) esTail
                      pure (A0TyList a0tyeFirst Nothing, ALitList (a0eFirst : a0esTail))
                LitVec ns -> do
                  let vec = Vector.fromList ns
                  pure (A0TyPrim (a0TyVec (Vector.length vec)) Nothing, ALitVec vec)
                LitMat nss -> do
                  mat <-
                    liftEither . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $
                      Matrix.fromRows nss
                  pure (A0TyPrim (uncurry a0TyMat (Matrix.size mat)) Nothing, ALitMat mat)
            pure (Pure a0tye, A0Literal alit)
          _ : _ ->
            typeError trav $ CannotApplyLiteral spanInFile
      Var (ms, x) -> do
        valEntry <- findValVar trav loc ms x tyEnv
        (a0tye, builtInNameOrSv) <-
          case valEntry of
            Ass0Entry a0tye' a0metadataOrSv ->
              pure $
                (a0tye',) $
                  case a0metadataOrSv of
                    Left Ass0Metadata {ass0builtInName} -> Left ass0builtInName
                    Right svx -> Right svx
            AssPersEntry aPtye AssPersMetadata {assPbuiltInName} ->
              pure (persistentTypeTo0 aPtye, Left (unliftBuiltInName assPbuiltInName))
            Ass1Entry _ _ ->
              typeError trav $ NotAStage0Var spanInFile x
        result <- instantiateGuidedByAppContext0 trav loc appCtx a0tye
        case builtInNameOrSv of
          Left builtInName ->
            pure (result, A0BuiltInName builtInName)
          Right svX -> do
            let ax = AssVarStatic svX
            pure (result, A0Var ax)
      Lam recOpt labelOpt (x1, tye1) e2 ->
        case appCtx of
          [] -> do
            svX1 <- generateFreshVar (Just x1)
            let ax1 = AssVarStatic svX1
            case recOpt of
              Nothing -> do
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (a0tye2, a0e2) <- do
                  let tyEnv' = TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1)) tyEnv
                  typecheckExpr0Single trav tyEnv' e2
                let sa0tye1 = strictify a0tye1
                pure (Pure (A0TyArrow labelOpt (Just ax1, a0tye1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
              Just (f, tyeRec) -> do
                svF <- generateFreshVar (Just f)
                let af = AssVarStatic svF
                a0tyeRec <- typecheckTypeExpr0 trav tyEnv tyeRec
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (a0tye2, a0e2) <- do
                  let tyEnv' =
                        tyEnv
                          & TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1))
                          & TypeEnv.addVal f (Ass0Entry a0tyeRec (Right svF))
                  typecheckExpr0Single trav tyEnv' e2
                let a0tyeSynth = A0TyArrow labelOpt (Just ax1, a0tye1) a0tye2
                (cast, _varSolution, _tyvar0Solution) <-
                  makeAssertiveCast trav loc Set.empty Set.empty a0tyeSynth a0tyeRec
                let sa0tyeRec = strictify a0tyeRec
                let sa0tye1 = strictify a0tye1
                pure (Pure a0tyeRec, applyCast cast (A0Lam (Just (af, sa0tyeRec)) (ax1, sa0tye1) a0e2))
          _ : _ ->
            -- TODO: consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamWithArguments appCtx
      App e1 labelOpt e2 -> do
        (a0tye2, a0e2) <- typecheckExpr0Single trav tyEnv e2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArg0 labelOpt a0e2 a0tye2 : appCtx) e1
        case result1 of
          Cast0 cast _a0tye11 result -> do
            pure (result, A0App a0e1 (applyCast cast a0e2))
          _ -> do
            bug "stage-0, App, fun"
      LamImp (x1, tye1) e2 -> do
        svX1 <- generateFreshVar (Just x1)
        let ax1 = AssVarStatic svX1
        case appCtx of
          [] -> do
            a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
            (a0tye2, a0e2) <- do
              let tyEnv' = TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1)) tyEnv
              typecheckExpr0Single trav tyEnv' e2
            let sa0tye1 = strictify a0tye1
            pure (Pure (A0TyImpArrow (ax1, a0tye1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
          _ : _ ->
            -- TODO: consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamImpWithArguments appCtx
      AppImpGiven e1 e2 -> do
        (a0tye2, a0e2) <- typecheckExpr0Single trav tyEnv e2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgImpGiven0 a0e2 a0tye2 : appCtx) e1
        case result1 of
          CastGiven0 cast _a0tye11 result -> do
            logImplicitArg $ LogGivenArg spanInFile a0e2
            pure (result, A0App a0e1 (applyCast cast a0e2))
          _ -> do
            bug "stage-0, AppImpGiven, not a CastGiven0"
      AppImpOmitted e1 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgImpOmitted0 : appCtx) e1
        case result1 of
          FillInferred0 a0eInferred result -> do
            logImplicitArg $ LogInferredArg spanInFile a0eInferred
            pure (result, A0App a0e1 a0eInferred)
          _ -> do
            bug "stage-0, AppImpOmitted, not a FillInferred0"
      LetIn x params e1 e2 -> do
        svX <- generateFreshVar (Just x)
        let ax = AssVarStatic svX
        (a0tye1, a0e1) <- typecheckLetInBody0 trav tyEnv params e1
        (result2, a0e2) <- do
          let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
          typecheckExpr0 trav tyEnv' appCtx e2
        let sa0tye1 = strictify a0tye1
        if ax `occurs0` result2
          then typeError trav $ VarOccursFreelyInAss0Type spanInFile x result2
          else pure (result2, A0LetIn (ax, sa0tye1) a0e1 a0e2)
      LetRecIn f params tyeBody eBody e2 -> do
        a0tye1Rec <- constructFunTypeExpr0 trav tyEnv params tyeBody
        (labelOpt, x0, tyeParam0, paramsRest) <-
          case params of
            MandatoryBinder labelOpt' (x0', tyeParam0') : paramsRest' ->
              pure (labelOpt', x0', tyeParam0', paramsRest')
            ImplicitBinder _ : _ ->
              typeError trav $ LetRecParamsCannotStartWithImplicit spanInFile
            [] ->
              typeError trav $ LetRecRequiresNonEmptyParams spanInFile
        svFInner <- generateFreshVar (Just f)
        let afInner = AssVarStatic svFInner
        svX0 <- generateFreshVar (Just x0)
        let ax0 = AssVarStatic svX0
        a0tyeParam0 <- typecheckTypeExpr0 trav tyEnv tyeParam0
        (a0tyeRestSynth, a0eRest) <- do
          let tyEnv' =
                tyEnv
                  & TypeEnv.addVal f (Ass0Entry a0tye1Rec (Right svFInner))
                  & TypeEnv.addVal x0 (Ass0Entry a0tyeParam0 (Right svX0))
          typecheckLetInBody0 trav tyEnv' paramsRest eBody
        let a0tye1Synth = A0TyArrow labelOpt (Just ax0, a0tyeParam0) a0tyeRestSynth
        (cast, _varSolution, _tyvar0Solution) <-
          makeAssertiveCast trav loc Set.empty Set.empty a0tye1Synth a0tye1Rec
        let a0e1 = applyCast cast (A0Lam (Just (afInner, strictify a0tye1Rec)) (ax0, strictify a0tyeParam0) a0eRest)
        svFOuter <- generateFreshVar (Just f)
        let afOuter = AssVarStatic svFOuter
        (result2, a0e2) <- do
          let tyEnv' = TypeEnv.addVal f (Ass0Entry a0tye1Rec (Right svFOuter)) tyEnv
          typecheckExpr0 trav tyEnv' appCtx e2
        if afOuter `occurs0` result2
          then
            typeError trav $ VarOccursFreelyInAss0Type spanInFile f result2
          else do
            pure (result2, A0LetIn (afOuter, strictify a0tye1Rec) a0e1 a0e2)
      LetTupleIn xL xR e1 e2 -> do
        (a0tye1, a0e1) <- typecheckExpr0Single trav tyEnv e1
        case a0tye1 of
          A0TyProduct a0tyeL a0tyeR -> do
            svXL <- generateFreshVar (Just xL)
            let axL = AssVarStatic svXL
            svXR <- generateFreshVar (Just xR)
            let axR = AssVarStatic svXR
            (result2, a0e2) <- do
              let tyEnv' =
                    tyEnv
                      & TypeEnv.addVal xL (Ass0Entry a0tyeL (Right svXL))
                      & TypeEnv.addVal xR (Ass0Entry a0tyeR (Right svXR))
              typecheckExpr0 trav tyEnv' appCtx e2
            pure (result2, A0LetTupleIn axL axR a0e1 a0e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotATupleAtStage0 spanInFile1 a0tye1
      LetOpenIn m e -> do
        case TypeEnv.findModule m tyEnv of
          Nothing ->
            typeError trav $ UnboundModule spanInFile m
          Just (ModuleEntry sigr) -> do
            let tyEnv' = TypeEnv.appendSigRecord tyEnv sigr
            typecheckExpr0 trav tyEnv' appCtx e
      Sequential e1 e2 -> do
        (a0tye1, a0e1) <- typecheckExpr0Single trav tyEnv e1
        case a0tye1 of
          A0TyPrim (A0TyPrimBase ATyPrimUnit) _maybePred -> do
            (result2, a0e2) <- typecheckExpr0 trav tyEnv appCtx e2
            pure (result2, A0Sequential a0e1 a0e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotAUnitTypeForStage0 spanInFile1 a0tye1
      Tuple e1 e2 -> do
        case appCtx of
          [] -> do
            (a0tye1, a0e1) <- typecheckExpr0Single trav tyEnv e1
            (a0tye2, a0e2) <- typecheckExpr0Single trav tyEnv e2
            pure (Pure (A0TyProduct a0tye1 a0tye2), (A0Tuple a0e1 a0e2))
          _ : _ -> do
            typeError trav $ CannotApplyTuple spanInFile
      IfThenElse e0 e1 e2 -> do
        (a0tye0, a0e0) <- typecheckExpr0Single trav tyEnv e0
        case a0tye0 of
          A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePred -> do
            (result1, a0e1) <- typecheckExpr0 trav tyEnv appCtx e1
            (result2, a0e2) <- typecheckExpr0 trav tyEnv appCtx e2
            result <- mergeResultsByConditional0 trav loc a0e0 result1 result2
            pure (result, A0IfThenElse a0e0 a0e1 a0e2)
          _ -> do
            let Expr loc0 _ = e0
            spanInFile0 <- askSpanInFile loc0
            typeError trav $ NotABoolTypeForStage0 spanInFile0 a0tye0
      As e1 tye2 ->
        case appCtx of
          [] -> do
            a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
            a0e1 <- forceExpr0 trav tyEnv a0tye2 e1
            pure (Pure a0tye2, a0e1)
          _ : _ ->
            typeError trav $ Unsupported spanInFile $ AsWithArguments appCtx
      Bracket e1 -> do
        (result1, a1e1) <- typecheckExpr1 trav tyEnv appCtx e1
        result <- mapMPure (pure . A0TyCode) result1
        pure (result, A0Bracket a1e1)
      Escape _ ->
        typeError trav $ CannotUseEscapeAtStage0 spanInFile
  where
    completeInferredImplicit spanInFile = go
      where
        go pair@(result, a0e) =
          case result of
            InsertInferred0 a0eInferred result' -> do
              logImplicitArg $ LogInferredArg spanInFile a0eInferred
              go (result', A0App a0e a0eInferred)
            InsertInferredType0 a0tyeInferred result' ->
              go (result', A0AppType a0e (strictify a0tyeInferred))
            _ ->
              pure pair

constructFunTypeExpr0 :: trav -> TypeEnv -> [LamBinder] -> TypeExpr -> M trav Ass0TypeExpr
constructFunTypeExpr0 trav tyEnv params tyeBody = do
  (tyEnv', f) <-
    foldM
      ( \(tyEnv0, f0) param ->
          case param of
            MandatoryBinder labelOpt (x, tye) -> do
              svX <- generateFreshVar (Just x)
              let ax = AssVarStatic svX
              a0tye <- typecheckTypeExpr0 trav tyEnv0 tye
              let tyEnv1 = TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv0
              let f1 = f0 . A0TyArrow labelOpt (Just ax, a0tye)
              pure (tyEnv1, f1)
            ImplicitBinder (x, tye) -> do
              svX <- generateFreshVar (Just x)
              let ax = AssVarStatic svX
              a0tye <- typecheckTypeExpr0 trav tyEnv0 tye
              let tyEnv1 = TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv0
              let f1 = f0 . A0TyImpArrow (ax, a0tye)
              pure (tyEnv1, f1)
      )
      (tyEnv, id)
      params
  a0tyeBody <- typecheckTypeExpr0 trav tyEnv' tyeBody
  pure $ f a0tyeBody

constructFunTypeExpr1 :: trav -> Span -> TypeEnv -> [LamBinder] -> TypeExpr -> M trav Ass1TypeExpr
constructFunTypeExpr1 trav loc tyEnv params tyeBody = do
  spanInFile <- askSpanInFile loc
  a0tyeBody <- typecheckTypeExpr1 trav tyEnv tyeBody
  foldrM
    ( \param a0tyeAcc ->
        case param of
          MandatoryBinder labelOpt (_x, tye) -> do
            a0tye <- typecheckTypeExpr1 trav tyEnv tye
            pure $ A1TyArrow labelOpt a0tye a0tyeAcc
          ImplicitBinder (_x, _tye) ->
            typeError trav $ CannotUseLamImpAtStage1 spanInFile
    )
    a0tyeBody
    params

typecheckLetInBody0 :: trav -> TypeEnv -> [LamBinder] -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckLetInBody0 trav tyEnv params e1 =
  case params of
    [] -> do
      (a0tye1, a0e1) <- typecheckExpr0Single trav tyEnv e1
      pure (a0tye1, a0e1)
    MandatoryBinder labelOpt (x, tye) : params' -> do
      a0tye <- typecheckTypeExpr0 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv) params' e1
      let ax = AssVarStatic svX
      pure (A0TyArrow labelOpt (Just ax, a0tye) a0tye', A0Lam Nothing (ax, strictify a0tye) a0e')
    ImplicitBinder (x, tye) : params' -> do
      a0tye <- typecheckTypeExpr0 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv) params' e1
      let ax = AssVarStatic svX
      pure (A0TyImpArrow (ax, a0tye) a0tye', A0Lam Nothing (ax, strictify a0tye) a0e')

typecheckExpr1Single :: trav -> TypeEnv -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckExpr1Single trav tyEnv e@(Expr loc _) = do
  (result1, a1e) <- typecheckExpr1 trav tyEnv [] e
  case result1 of
    Pure a1tye ->
      pure (a1tye, a1e)
    _ -> do
      spanInFile <- askSpanInFile loc
      bug $ "non-empty result1: " ++ show spanInFile

typecheckExpr1 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result1, Ass1Expr)
typecheckExpr1 trav tyEnv appCtx (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  completeInferredImplicit
    <$> case eMain of
      Literal lit ->
        case appCtx of
          [] -> do
            (a1tye, alit) <-
              case lit of
                LitInt n ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimInt), ALitInt n)
                LitFloat r ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimFloat), ALitFloat r)
                LitUnit ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimUnit), ALitUnit)
                LitBool b ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimBool), ALitBool b)
                LitString t ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimString), ALitString t)
                LitList es ->
                  case es of
                    [] ->
                      typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
                    eFirst : esTail -> do
                      (a1tyeFirst, a1eFirst) <- typecheckExpr1Single trav tyEnv eFirst
                      a1esTail <-
                        mapM
                          ( \e@(Expr locElem _) -> do
                              (a1tye, a1e) <- typecheckExpr1Single trav tyEnv e
                              (eq, _varSolution, _tyvar1Solution) <-
                                makeEquation1 trav locElem Set.empty Set.empty a1tye a1tyeFirst
                              pure (applyEquationCast locElem eq a1e)
                          )
                          esTail
                      pure (A1TyList a1tyeFirst, ALitList (a1eFirst : a1esTail))
                LitVec ns -> do
                  let vec = Vector.fromList ns
                  pure (A1TyPrim (a1TyVec (A0Literal (ALitInt (Vector.length vec)))), ALitVec vec)
                LitMat nss -> do
                  mat <-
                    liftEither . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $
                      Matrix.fromRows nss
                  pure (A1TyPrim (uncurry a1TyMat (both (A0Literal . ALitInt) (Matrix.size mat))), ALitMat mat)
            pure (Pure a1tye, A1Literal alit)
          _ : _ ->
            typeError trav $ CannotApplyLiteral spanInFile
      Var (ms, x) -> do
        valEntry <- findValVar trav loc ms x tyEnv
        (a1tye, a1builtInNameOrSv) <-
          case valEntry of
            Ass0Entry _ _ ->
              typeError trav $ NotAStage1Var spanInFile x
            AssPersEntry aPtye AssPersMetadata {assPbuiltInName} ->
              pure (persistentTypeTo1 aPtye, Left assPbuiltInName)
            Ass1Entry a1tye' a1metadataOrSv ->
              pure $
                (a1tye',) $
                  case a1metadataOrSv of
                    Left Ass1Metadata {ass1builtInName} -> Left ass1builtInName
                    Right svX -> Right svX
        (result, _) <- instantiateGuidedByAppContext1 trav loc Set.empty appCtx a1tye
        case a1builtInNameOrSv of
          Left a1builtInName ->
            pure (result, A1BuiltInName a1builtInName)
          Right svX -> do
            let ax = AssVarStatic svX
            pure (result, A1Var ax)
      Lam recOpt labelOpt (x1, tye1) e2 ->
        case appCtx of
          [] -> do
            svX1 <- generateFreshVar (Just x1)
            case recOpt of
              Nothing -> do
                a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
                (a1tye2, a1e2) <- do
                  let tyEnv' = TypeEnv.addVal x1 (Ass1Entry a1tye1 (Right svX1)) tyEnv
                  typecheckExpr1Single trav tyEnv' e2
                let ax1 = AssVarStatic svX1
                pure (Pure (A1TyArrow labelOpt a1tye1 a1tye2), A1Lam Nothing (ax1, a1tye1) a1e2)
              Just (f, tyeRec) -> do
                svF <- generateFreshVar (Just f)
                a1tyeRec <- typecheckTypeExpr1 trav tyEnv tyeRec
                a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
                (a1tye2, a1e2) <- do
                  let tyEnv' =
                        tyEnv
                          & TypeEnv.addVal x1 (Ass1Entry a1tye1 (Right svX1))
                          & TypeEnv.addVal f (Ass1Entry a1tyeRec (Right svF))
                  typecheckExpr1Single trav tyEnv' e2
                let ax1 = AssVarStatic svX1
                let af = AssVarStatic svF
                let a1tyeSynth = A1TyArrow labelOpt a1tye1 a1tye2
                (eq, _varSolution, _tyvar1Solution) <-
                  makeEquation1 trav loc Set.empty Set.empty a1tyeSynth a1tyeRec
                pure (Pure a1tyeRec, applyEquationCast loc eq (A1Lam (Just (af, a1tyeRec)) (ax1, a1tye1) a1e2))
          _ : _ ->
            -- TODO: consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamWithArguments appCtx
      App e1 labelOpt e2 -> do
        (a1tye2, a1e2) <- typecheckExpr1Single trav tyEnv e2
        (result1, a1e1) <- typecheckExpr1 trav tyEnv (AppArg1 labelOpt a1tye2 : appCtx) e1
        case result1 of
          Cast1 cast _a1tye11 result ->
            -- Embeds type equality assertion at stage 0 here!
            pure (result, A1App a1e1 (applyCast1 cast a1e2))
          _ ->
            bug "stage-1, App, fun, not a Cast1"
      LamImp _ _ ->
        typeError trav $ CannotUseLamImpAtStage1 spanInFile
      AppImpGiven _ _ ->
        typeError trav $ CannotUseAppImpGivenAtStage1 spanInFile
      AppImpOmitted _ ->
        typeError trav $ CannotUseAppImpOmittedAtStage1 spanInFile
      LetIn x params eBody e2 -> do
        svX <- generateFreshVar (Just x)
        (a1tye1, a1e1) <- typecheckLetInBody1 trav tyEnv params eBody
        (result2, a1e2) <-
          typecheckExpr1 trav (TypeEnv.addVal x (Ass1Entry a1tye1 (Right svX)) tyEnv) appCtx e2
        let ax = AssVarStatic svX
        if ax `occurs1` result2
          then typeError trav $ VarOccursFreelyInAss1Type spanInFile x result2
          else pure (result2, A1LetIn (ax, a1tye1) a1e1 a1e2)
      LetRecIn f params tyeBody eBody e2 -> do
        a1tye1Rec <- constructFunTypeExpr1 trav loc tyEnv params tyeBody
        (labelOpt, x0, tyeParam0, paramsRest) <-
          case params of
            MandatoryBinder labelOpt' (x0', tyeParam0') : paramsRest' ->
              pure (labelOpt', x0', tyeParam0', paramsRest')
            ImplicitBinder _ : _ ->
              typeError trav $ LetRecParamsCannotStartWithImplicit spanInFile
            [] ->
              typeError trav $ LetRecRequiresNonEmptyParams spanInFile
        svFInner <- generateFreshVar (Just f)
        let afInner = AssVarStatic svFInner
        svX0 <- generateFreshVar (Just x0)
        let ax0 = AssVarStatic svX0
        a1tyeParam0 <- typecheckTypeExpr1 trav tyEnv tyeParam0
        (a1tyeRestSynth, a1eRest) <- do
          let tyEnv' =
                tyEnv
                  & TypeEnv.addVal f (Ass1Entry a1tye1Rec (Right svFInner))
                  & TypeEnv.addVal x0 (Ass1Entry a1tyeParam0 (Right svX0))
          typecheckLetInBody1 trav tyEnv' paramsRest eBody
        let a1tye1Synth = A1TyArrow labelOpt a1tyeParam0 a1tyeRestSynth
        (eq, _varSolution, _tyvar1Solution) <- makeEquation1 trav loc Set.empty Set.empty a1tye1Synth a1tye1Rec
        let a1e1 = applyEquationCast loc eq (A1Lam (Just (afInner, a1tye1Rec)) (ax0, a1tyeParam0) a1eRest)
        svFOuter <- generateFreshVar (Just f)
        let afOuter = AssVarStatic svFOuter
        (result2, a1e2) <- do
          let tyEnv' = TypeEnv.addVal f (Ass1Entry a1tye1Rec (Right svFOuter)) tyEnv
          typecheckExpr1 trav tyEnv' appCtx e2
        if afOuter `occurs1` result2
          then typeError trav $ VarOccursFreelyInAss1Type spanInFile f result2
          else pure (result2, A1LetIn (afOuter, a1tye1Rec) a1e1 a1e2)
      LetTupleIn xL xR e1 e2 -> do
        (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
        case a1tye1 of
          A1TyProduct a1tyeL a1tyeR -> do
            svXL <- generateFreshVar (Just xL)
            let axL = AssVarStatic svXL
            svXR <- generateFreshVar (Just xR)
            let axR = AssVarStatic svXR
            (result2, a1e2) <- do
              let tyEnv' =
                    tyEnv
                      & TypeEnv.addVal xL (Ass1Entry a1tyeL (Right svXL))
                      & TypeEnv.addVal xR (Ass1Entry a1tyeR (Right svXR))
              typecheckExpr1 trav tyEnv' appCtx e2
            pure (result2, A1LetTupleIn axL axR a1e1 a1e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotATupleAtStage1 spanInFile1 a1tye1
      LetOpenIn m e -> do
        case TypeEnv.findModule m tyEnv of
          Nothing ->
            typeError trav $ UnboundModule spanInFile m
          Just (ModuleEntry sigr) -> do
            let tyEnv' = TypeEnv.appendSigRecord tyEnv sigr
            typecheckExpr1 trav tyEnv' appCtx e
      Sequential e1 e2 -> do
        (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
        case a1tye1 of
          A1TyPrim (A1TyPrimBase ATyPrimUnit) -> do
            (result2, a1e2) <- typecheckExpr1 trav tyEnv appCtx e2
            pure (result2, A1Sequential a1e1 a1e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotAUnitTypeForStage1 spanInFile1 a1tye1
      Tuple e1 e2 -> do
        case appCtx of
          [] -> do
            (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
            (a1tye2, a1e2) <- typecheckExpr1Single trav tyEnv e2
            pure (Pure (A1TyProduct a1tye1 a1tye2), A1Tuple a1e1 a1e2)
          _ : _ ->
            typeError trav $ CannotApplyTuple spanInFile
      IfThenElse e0 e1 e2 -> do
        (a1tye0, a1e0) <- typecheckExpr1Single trav tyEnv e0
        case a1tye0 of
          A1TyPrim (A1TyPrimBase ATyPrimBool) ->
            case appCtx of
              [] -> do
                (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
                (a1tye2, a1e2) <- typecheckExpr1Single trav tyEnv e2
                (eq, _varSolution, _tyvar1Solution) <- makeEquation1 trav loc Set.empty Set.empty a1tye2 a1tye1
                pure (Pure a1tye1, A1IfThenElse a1e0 a1e1 (applyEquationCast loc eq a1e2))
              _ : _ -> do
                typeError trav $ Stage1IfThenElseRestrictedToEmptyContext spanInFile appCtx
          _ -> do
            let Expr loc0 _ = e0
            spanInFile0 <- askSpanInFile loc0
            typeError trav $ NotABoolTypeForStage1 spanInFile0 a1tye0
      As e1 tye2 ->
        -- The following is a very ad-hoc branching.
        -- TODO: fix this with "truly" bidirectional type-checking
        case e1 of
          Expr _loc1 (Literal (LitList [])) ->
            case appCtx of
              [] -> do
                a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
                pure (Pure a1tye2, A1Literal (ALitList []))
              _ : _ ->
                typeError trav $ CannotApplyLiteral spanInFile
          _ -> do
            (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
            a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
            (eq, _varSolution, _tyvar1Solution) <- makeEquation1 trav loc Set.empty Set.empty a1tye1 a1tye2
            pure (Pure a1tye2, applyEquationCast loc eq a1e1)
      Bracket _ ->
        typeError trav $ CannotUseBracketAtStage1 spanInFile
      Escape e1 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv appCtx e1
        result <-
          mapMPure
            ( \case
                A0TyCode a1tye1 ->
                  pure a1tye1
                a0tye1 -> do
                  let Expr loc1 _ = e1
                  spanInFile1 <- askSpanInFile loc1
                  typeError trav $ NotACodeType spanInFile1 a0tye1
            )
            result1
        pure (result, A1Escape a0e1)
  where
    completeInferredImplicit pair@(result, a1e) =
      case result of
        InsertType1 a1tyeInferred result' ->
          completeInferredImplicit (result', A1AppType a1e a1tyeInferred)
        _ ->
          pair

typecheckLetInBody1 :: trav -> TypeEnv -> [LamBinder] -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckLetInBody1 trav tyEnv params e1 =
  case params of
    [] -> do
      (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
      pure (a1tye1, a1e1)
    MandatoryBinder labelOpt (x, tye) : params' -> do
      a1tye <- typecheckTypeExpr1 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a1tye', a1e') <- typecheckLetInBody1 trav (TypeEnv.addVal x (Ass1Entry a1tye (Right svX)) tyEnv) params' e1
      let ax = AssVarStatic svX
      pure (A1TyArrow labelOpt a1tye a1tye', A1Lam Nothing (ax, a1tye) a1e')
    ImplicitBinder (_x, tye) : _params' -> do
      let TypeExpr loc _ = tye -- TODO (enhance): give a better code position
      spanInFile <- askSpanInFile loc
      typeError trav $ CannotUseLamImpAtStage1 spanInFile

mapMPure :: (af StaticVar -> M trav (bf StaticVar)) -> ResultF af StaticVar -> M trav (ResultF bf StaticVar)
mapMPure f = go
  where
    go (Pure v) = Pure <$> f v
    go (Cast0 cast a0tye r) = Cast0 cast a0tye <$> go r
    go (Cast1 eq a1tye r) = Cast1 eq a1tye <$> go r
    go (CastGiven0 a0e a0tye r) = CastGiven0 a0e a0tye <$> go r
    go (FillInferred0 a0e r) = FillInferred0 a0e <$> go r
    go (InsertInferred0 a0e r) = InsertInferred0 a0e <$> go r
    go (InsertInferredType0 a0tye r) = InsertInferredType0 a0tye <$> go r
    go (InsertType1 a1tye r) = InsertType1 a1tye <$> go r

validateExprArg0 :: trav -> (IntermediateArgForAss0Type, Span) -> M trav Ass0Expr
validateExprArg0 trav = \case
  (IA0ExprArg a0e, _loc) ->
    pure a0e
  (IA0TypeArg _a0tye, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAValueArg spanInFile

validateTypeArg0 :: trav -> (IntermediateArgForAss0Type, Span) -> M trav Ass0TypeExpr
validateTypeArg0 trav = \case
  (IA0ExprArg _, loc) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotATypeArg spanInFile
  (IA0TypeArg a0tye, _loc) ->
    pure a0tye

validateIntLiteral :: trav -> Span -> Ass0Expr -> M trav Int
validateIntLiteral trav loc a0e =
  case a0e of
    A0Literal (ALitInt n) ->
      pure n
    _ -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ NotAnIntLitArgAtStage0 spanInFile a0e

validateIntListLiteral :: trav -> Span -> Ass0Expr -> M trav [Int]
validateIntListLiteral trav loc a0e =
  case a0e of
    A0Literal (ALitList a0es) -> do
      spanInFile <- askSpanInFile loc
      mapM
        ( \case
            A0Literal (ALitInt n) -> pure n
            _ -> typeError trav $ NotAnIntListLitArgAtStage0 spanInFile a0e
        )
        a0es
    _ -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ NotAnIntListLitArgAtStage0 spanInFile a0e

data IntermediateArgForAss0Type
  = IA0ExprArg Ass0Expr
  | IA0TypeArg Ass0TypeExpr

typecheckTypeExpr0 :: trav -> TypeEnv -> TypeExpr -> M trav Ass0TypeExpr
typecheckTypeExpr0 trav tyEnv (TypeExpr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    TyName tyName args -> do
      results <-
        mapM
          ( \case
              ExprArgPersistent (Expr loc' _) -> do
                spanInFile' <- askSpanInFile loc'
                typeError trav $ CannotUsePersistentArgAtStage0 spanInFile'
              ExprArgNormal e@(Expr loc' _) -> do
                (_a0tye, a0e) <- typecheckExpr0Single trav tyEnv e
                pure (IA0ExprArg a0e, loc')
              TypeArg tye@(TypeExpr loc' _) -> do
                a0tye <- typecheckTypeExpr0 trav tyEnv tye
                pure (IA0TypeArg a0tye, loc')
          )
          args
      case (tyName, results) of
        ("Nat", []) ->
          pure BuiltIn.tyNat
        (_, []) ->
          case validatePrimBaseType tyName of
            Just tyPrimBase -> pure $ A0TyPrim (A0TyPrimBase tyPrimBase) Nothing
            Nothing -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile tyName 0
        ("List", [arg]) -> do
          a0tye <- validateTypeArg0 trav arg
          pure $ A0TyList a0tye Nothing
        ("Vec", [arg1@(_, loc1)]) -> do
          a0e1 <- validateExprArg0 trav arg1
          n1 <- validateIntLiteral trav loc1 a0e1
          pure $ A0TyPrim (a0TyVec n1) Nothing
        ("Mat", [arg1@(_, loc1), arg2@(_, loc2)]) -> do
          a0e1 <- validateExprArg0 trav arg1
          a0e2 <- validateExprArg0 trav arg2
          n1 <- validateIntLiteral trav loc1 a0e1
          n2 <- validateIntLiteral trav loc2 a0e2
          pure $ A0TyPrim (a0TyMat n1 n2) Nothing
        ("Tensor", [arg@(_, loc')]) -> do
          a0e <- validateExprArg0 trav arg
          ns <- validateIntListLiteral trav loc' a0e
          pure $ A0TyPrim (A0TyTensor ns) Nothing
        ("Dataset", [arg1@(_, loc1), arg2@(_, loc2), arg3@(_, loc3), arg4@(_, loc4)]) -> do
          a0e1 <- validateExprArg0 trav arg1
          a0e2 <- validateExprArg0 trav arg2
          a0e3 <- validateExprArg0 trav arg3
          a0e4 <- validateExprArg0 trav arg4
          numTrain <- validateIntLiteral trav loc1 a0e1
          numTest <- validateIntLiteral trav loc2 a0e2
          image <- validateIntListLiteral trav loc3 a0e3
          label <- validateIntListLiteral trav loc4 a0e4
          let datasetParam = DatasetParam {numTrain, numTest, image, label}
          pure $ A0TyPrim (A0TyDataset datasetParam) Nothing
        ("Lstm", [arg1@(_, loc1), arg2@(_, loc2)]) -> do
          a0e1 <- validateExprArg0 trav arg1
          a0e2 <- validateExprArg0 trav arg2
          inputSize <- validateIntLiteral trav loc1 a0e1
          hiddenSize <- validateIntLiteral trav loc2 a0e2
          pure $ A0TyPrim (A0TyLstm inputSize hiddenSize) Nothing
        ("TextHelper", [arg1@(_, loc1)]) -> do
          a0e1 <- validateExprArg0 trav arg1
          labels <- validateIntLiteral trav loc1 a0e1
          pure $ A0TyPrim (A0TyTextHelper labels) Nothing
        _ ->
          typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile tyName (List.length results)
    TyVar tyvar -> do
      tyvarEntry <- findTypeVar trav loc tyvar tyEnv
      case tyvarEntry of
        TypeVarEntry0 atyvar -> pure $ A0TyVar atyvar
        TypeVarEntry1 _ -> typeError trav $ NotAStage0TypeVar spanInFile tyvar
    TyArrow labelOpt (xOpt, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      (tyEnv', svXOpt) <-
        case xOpt of
          Just x -> do
            svX <- generateFreshVar (Just x)
            pure (TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv, Just svX)
          Nothing ->
            pure (tyEnv, Nothing)
      a0tye2 <- typecheckTypeExpr0 trav tyEnv' tye2
      let axOpt = AssVarStatic <$> svXOpt
      pure $ A0TyArrow labelOpt (axOpt, a0tye1) a0tye2
    TyCode tye1 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      pure $ A0TyCode a1tye1
    TyImpArrow (x, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      svX <- generateFreshVar (Just x)
      a0tye2 <- do
        let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
        typecheckTypeExpr0 trav tyEnv' tye2
      let ax = AssVarStatic svX
      pure $ A0TyImpArrow (ax, a0tye1) a0tye2
    TyRefinement x tye1 e2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      svX <- generateFreshVar (Just x)
      (a0tye2, a0e2) <- do
        let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
        typecheckExpr0Single trav tyEnv' e2
      case a0tye2 of
        A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePredForBool -> do
          let ax = AssVarStatic svX
          case a0tye1 of
            A0TyPrim a0tyPrim Nothing -> do
              pure $
                A0TyPrim a0tyPrim . Just $
                  A0Lam Nothing (ax, strictify a0tye1) a0e2
            A0TyPrim a0tyPrim (Just a0ePredForBase) -> do
              pure $
                A0TyPrim a0tyPrim . Just $
                  A0Lam Nothing (ax, strictify a0tye1) $
                    A0App (A0App ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
            A0TyList a0tyeElem Nothing -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) a0e2
            A0TyList a0tyeElem (Just a0ePredForBase) -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) $
                    A0App (A0App ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
            _ -> do
              let TypeExpr loc1 _ = tye1
              spanInFile1 <- askSpanInFile loc1
              typeError trav $ InvalidTypeForRefinement spanInFile1 a0tye1
        _ -> do
          let Expr loc2 _ = e2
          spanInFile2 <- askSpanInFile loc2
          typeError trav $ NotABoolTypeForStage0 spanInFile2 a0tye2
    TyProduct tye1 tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
      pure $ A0TyProduct a0tye1 a0tye2
    TyForAll tyvar tye1 -> do
      atyvar <- generateFreshTypeVar tyvar
      a0tye1 <- do
        let tyEnv' = TypeEnv.addTypeVar tyvar (TypeVarEntry0 atyvar) tyEnv
        typecheckTypeExpr0 trav tyEnv' tye1
      pure $ A0TyImplicitForAll atyvar a0tye1

ass0exprAnd :: Ass0Expr
ass0exprAnd = A0BuiltInName (BuiltInArity2 BIAnd)

ass0exprListMap :: Ass0Expr
ass0exprListMap = A0BuiltInName (BuiltInArity2 BIListMap)

validatePersistentExprArg1 :: trav -> ArgForType -> M trav Expr
validatePersistentExprArg1 trav = \case
  ExprArgPersistent e ->
    pure e
  ExprArgNormal (Expr loc _) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ CannotUseNormalArgAtStage1 spanInFile
  TypeArg (TypeExpr loc _) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotAValueArg spanInFile

validateTypeArg1 :: trav -> ArgForType -> M trav TypeExpr
validateTypeArg1 trav = \case
  ExprArgPersistent (Expr loc _) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ NotATypeArg spanInFile
  ExprArgNormal (Expr loc _) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ CannotUseNormalArgAtStage1 spanInFile
  TypeArg tye ->
    pure tye

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav Ass1TypeExpr
typecheckTypeExpr1 trav tyEnv (TypeExpr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    TyName tyName args -> do
      case (tyName, args) of
        (_, []) ->
          case validatePrimBaseType tyName of
            Just tyPrimBase -> pure $ A1TyPrim (A1TyPrimBase tyPrimBase)
            Nothing -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile tyName 0
        ("List", [arg]) -> do
          tye <- validateTypeArg1 trav arg
          a1tye <- typecheckTypeExpr1 trav tyEnv tye
          pure $ A1TyList a1tye
        ("Vec", [arg]) -> do
          e <- validatePersistentExprArg1 trav arg
          a0e <- forceExpr0 trav tyEnv BuiltIn.tyNat e
          pure $ A1TyPrim (a1TyVec a0e)
        ("Mat", [arg1, arg2]) -> do
          e1 <- validatePersistentExprArg1 trav arg1
          e2 <- validatePersistentExprArg1 trav arg2
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat e2
          pure $ A1TyPrim (a1TyMat a0e1 a0e2)
        ("Tensor", [arg]) -> do
          e <- validatePersistentExprArg1 trav arg
          a0eList <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) e
          pure $ A1TyPrim (A1TyTensor a0eList)
        ("Dataset", [arg1, arg2, arg3, arg4]) -> do
          e1 <- validatePersistentExprArg1 trav arg1
          e2 <- validatePersistentExprArg1 trav arg2
          e3 <- validatePersistentExprArg1 trav arg3
          e4 <- validatePersistentExprArg1 trav arg4
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat e2
          a0e3 <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) e3
          a0e4 <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) e4
          let datasetParam =
                DatasetParam
                  { numTrain = a0e1,
                    numTest = a0e2,
                    image = Identity a0e3,
                    label = Identity a0e4
                  }
          pure $ A1TyPrim (A1TyDataset datasetParam)
        ("Lstm", [arg1, arg2]) -> do
          e1 <- validatePersistentExprArg1 trav arg1
          e2 <- validatePersistentExprArg1 trav arg2
          a0eInputSize <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          a0eHiddenSize <- forceExpr0 trav tyEnv BuiltIn.tyNat e2
          pure $ A1TyPrim (A1TyLstm a0eInputSize a0eHiddenSize)
        ("TextHelper", [arg1]) -> do
          e1 <- validatePersistentExprArg1 trav arg1
          a0eLabels <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          pure $ A1TyPrim (A1TyTextHelper a0eLabels)
        _ ->
          typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile tyName (List.length args)
    TyVar _tyvar ->
      typeError trav $ CannotUseTypeVarAtStage1 spanInFile
    TyArrow labelOpt (xOpt, tye1) tye2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      () <-
        case xOpt of
          Nothing -> pure ()
          Just x -> typeError trav $ FunctionTypeCannotBeDependentAtStage1 spanInFile x
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      pure $ A1TyArrow labelOpt a1tye1 a1tye2
    TyImpArrow _ _ ->
      typeError trav $ CannotUseImpArrowTypeAtStage1 spanInFile
    TyCode _ -> do
      typeError trav $ CannotUseCodeTypeAtStage1 spanInFile
    TyRefinement _ _ _ -> do
      typeError trav $ CannotUseRefinementTypeAtStage1 spanInFile
    TyProduct tye1 tye2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
      pure $ A1TyProduct a1tye1 a1tye2
    TyForAll tyvar tye1 -> do
      atyvar <- generateFreshTypeVar tyvar
      a1tye1 <- do
        let tyEnv' = TypeEnv.addTypeVar tyvar (TypeVarEntry1 atyvar) tyEnv
        typecheckTypeExpr1 trav tyEnv' tye1
      pure $ A1TyImplicitForAll atyvar a1tye1

validatePersistentType :: trav -> Span -> Ass0TypeExpr -> M trav AssPersTypeExpr
validatePersistentType trav loc a0tye =
  case go a0tye of
    Just aPtye ->
      pure aPtye
    Nothing -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ InvalidPersistentType spanInFile a0tye
  where
    go = \case
      A0TyPrim a0tyPrim maybePred ->
        case maybePred of
          Nothing -> pure $ APersTyPrim a0tyPrim
          Just _ -> Nothing
      A0TyVar atyvar ->
        pure $ APersTyVar atyvar
      A0TyList a0tye' maybePred ->
        case maybePred of
          Nothing -> APersTyList <$> go a0tye'
          Just _ -> Nothing
      A0TyProduct a0tye1 a0tye2 ->
        APersTyProduct <$> go a0tye1 <*> go a0tye2
      A0TyArrow labelOpt (Nothing, a0tye1) a0tye2 -> do
        aPtye1 <- go a0tye1
        aPtye2 <- go a0tye2
        pure $ APersTyArrow labelOpt aPtye1 aPtye2
      A0TyArrow _labelOpt (Just _x, _a0tye1) _a0tye2 -> do
        Nothing
      A0TyImpArrow (_x, _a0tye1) _a0tye2 -> do
        Nothing
      A0TyCode _ ->
        Nothing
      A0TyImplicitForAll atyvar a0tye' -> do
        aPtye' <- go a0tye'
        pure $ APersTyImplicitForAll atyvar aPtye'

extractFromExternal :: ExternalField -> External -> Maybe Text
extractFromExternal field0 =
  List.firstJust (\(field, s) -> if field == field0 then Just s else Nothing)

typecheckBind :: trav -> TypeEnv -> Bind -> M trav (SigRecord, [AssBind])
typecheckBind trav tyEnv (Bind loc bindMain) =
  case bindMain of
    BindVal stage x (BindValExternal tye ext) -> do
      extName <-
        case extractFromExternal "builtin" ext of
          Just s ->
            pure s
          Nothing -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ NoBuiltInNameInExternal spanInFile
      let surfaceName = extractFromExternal "surface" ext
      case stage of
        Stage0 -> do
          a0tye <- typecheckTypeExpr0 trav tyEnv tye
          ass0builtInName <-
            case validateExternalName0 extName of
              Just a0builtInName' ->
                pure a0builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          let a0metadata = Ass0Metadata {ass0builtInName, ass0surfaceName = surfaceName}
          pure (SigRecord.singletonVal x (Ass0Entry a0tye (Left a0metadata)), [])
        Stage1 -> do
          ass1builtInName <-
            case validateExternalName1 extName of
              Just ass1builtInName' ->
                pure ass1builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          a1tye <- typecheckTypeExpr1 trav tyEnv tye
          let a1metadata = Ass1Metadata {ass1builtInName, ass1surfaceName = surfaceName}
          pure (SigRecord.singletonVal x (Ass1Entry a1tye (Left a1metadata)), [])
        StagePers -> do
          a0tye <- typecheckTypeExpr0 trav tyEnv tye
          aPtye <- validatePersistentType trav loc a0tye
          assPbuiltInName <-
            case validateExternalName1 extName of
              Just a1builtInName' ->
                pure a1builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          let aPmetadata = AssPersMetadata {assPbuiltInName, assPsurfaceName = surfaceName}
          pure (SigRecord.singletonVal x (AssPersEntry aPtye aPmetadata), [])
    BindVal stage x (BindValNormal e) -> do
      svX <- generateFreshVar (Just x)
      let ax = AssVarStatic svX
      case stage of
        Stage0 -> do
          (a0tye, a0e) <- typecheckExpr0Single trav tyEnv e
          let sa0tye = strictify a0tye
          pure (SigRecord.singletonVal x (Ass0Entry a0tye (Right svX)), [ABind0 (ax, sa0tye) a0e])
        Stage1 -> do
          (a1tye, a1e) <- typecheckExpr1Single trav tyEnv e
          pure (SigRecord.singletonVal x (Ass1Entry a1tye (Right svX)), [ABind1 (ax, a1tye) a1e])
        StagePers -> do
          -- TODO: bind persistent values
          spanInFile <- askSpanInFile loc
          typeError trav $ Unsupported spanInFile (CannotBindPersistentValue x)
    BindModule m binds -> do
      (_, sigr, abinds) <- typecheckBinds trav tyEnv binds
      pure (SigRecord.singletonModule m (ModuleEntry sigr), abinds)

typecheckBinds :: trav -> TypeEnv -> [Bind] -> M trav (TypeEnv, SigRecord, [AssBind])
typecheckBinds trav tyEnv =
  foldM
    ( \(tyEnv', sigr', abinds') bind@(Bind loc _) -> do
        (sigr, abinds) <- typecheckBind trav tyEnv' bind
        case SigRecord.intersection sigr' sigr of
          ([], []) ->
            pure (TypeEnv.appendSigRecord tyEnv' sigr, SigRecord.union sigr' sigr, abinds' ++ abinds)
          (x : _, _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile x
          (_, m : _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile m
    )
    (tyEnv, SigRecord.empty, [])
