{-# LANGUAGE TemplateHaskell #-}

module Staged.Evaluator
  ( evalExpr0,
    evalExpr1,
    initialState,
    run,
    unliftVal,
    EvalState,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Data.Function ((&))
import Data.Functor.Identity
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Text qualified as Text
import Staged.BuiltIn.CompileTime (deriveDeltaReduction)
import Staged.BuiltIn.Core
import Staged.BuiltIn.Definitions (definitions)
import Staged.EvalError
import Staged.Syntax
import Util.LocationInFile (SourceSpec, getSpanInFile)
import Util.Matrix (Matrix)
import Util.Vector (Vector)
import Prelude

data EvalState = EvalState
  { nextSymbolIndex :: Int,
    sourceSpec :: SourceSpec -- For assertion failure
  }

type M a = StateT EvalState (Either EvalError) a

evalError :: EvalError -> M a
evalError = lift . Left

bug :: Bug -> M a
bug = lift . Left . Bug

initialState :: SourceSpec -> EvalState
initialState sourceSpec =
  EvalState {nextSymbolIndex = 0, sourceSpec}

generateFreshSymbol :: M Symbol
generateFreshSymbol = do
  currentState@EvalState {nextSymbolIndex} <- get
  put $ currentState {nextSymbolIndex = nextSymbolIndex + 1}
  pure $ Symbol nextSymbolIndex

generateIdentityFunction :: EvalEnv -> Ass0TypeVal -> M Ass0Val
generateIdentityFunction env a0tyv = do
  x <- symbolToVar <$> generateFreshSymbol
  pure $ A0ValLam Nothing (x, a0tyv) (A0Var x) env

findEntry :: EvalEnv -> AssVar -> M EvalEnvEntry
findEntry env x =
  case Map.lookup x env of
    Nothing -> bug $ UnboundVarFound x
    Just envEntry -> pure envEntry

findVal0 :: EvalEnv -> AssVar -> M Ass0Val
findVal0 env x = do
  entry <- findEntry env x
  case entry of
    Ass0ValEntry a0v -> pure a0v
    SymbolEntry symb -> bug $ FoundSymbol x symb

findSymbol :: EvalEnv -> AssVar -> M Symbol
findSymbol env x = do
  entry <- findEntry env x
  case entry of
    Ass0ValEntry a0v -> bug $ FoundAss0Val x a0v
    SymbolEntry symb -> pure symb

validateIntLiteral :: Ass0Val -> M Int
validateIntLiteral = \case
  A0ValLiteral (ALitInt n) -> pure n
  a0v -> bug $ NotAnInteger a0v

validateBoolLiteral :: Ass0Val -> M Bool
validateBoolLiteral = \case
  A0ValLiteral (ALitBool b) -> pure b
  a0v -> bug $ NotABoolean a0v

validateUnitLiteral :: Ass0Val -> M ()
validateUnitLiteral = \case
  A0ValLiteral ALitUnit -> pure ()
  a0v -> bug $ NotAUnit a0v

{-
validateStringLiteral :: Ass0Val -> M Text
validateStringLiteral = \case
  A0ValLiteral (ALitString s) -> pure s
  a0v -> bug $ NotAString a0v
-}

validateTupleValue :: Ass0Val -> M (Ass0Val, Ass0Val)
validateTupleValue = \case
  A0ValTuple a0v1 a0v2 -> pure (a0v1, a0v2)
  a0v -> bug $ NotATuple a0v

validateListValue :: Ass0Val -> M [Ass0Val]
validateListValue = \case
  A0ValLiteral (ALitList a0vs) -> pure a0vs
  a0v -> bug $ NotAList a0v

validateIntListLiteral :: Ass0Val -> M [Int]
validateIntListLiteral a0v = do
  a0vs <- validateListValue a0v
  mapM validateIntLiteral a0vs

validateIntPairLiteral :: Ass0Val -> M (Int, Int)
validateIntPairLiteral a0v = do
  (a0v1, a0v2) <- validateTupleValue a0v
  n1 <- validateIntLiteral a0v1
  n2 <- validateIntLiteral a0v2
  pure (n1, n2)

discardValue :: Ass0Val -> M ()
discardValue = const $ pure ()

validateVec0 :: Ass0Val -> M Vector
validateVec0 = \case
  A0ValLiteral (ALitVec v) -> pure v
  a0v -> bug $ NotAVector a0v

validateMat0 :: Ass0Val -> M Matrix
validateMat0 = \case
  A0ValLiteral (ALitMat mat) -> pure mat
  a0v -> bug $ NotAMatrix a0v

-- The implementation of the built-in function `drop_at`.
dropAt :: Int -> [a] -> [a]
dropAt _ [] = []
dropAt n (v : vs) = if n <= 0 then vs else v : dropAt (n - 1) vs

-- The implementation of the built-in function `drop_at`.
broadcast :: [Int] -> [Int] -> Maybe [Int]
broadcast ns1' ns2' = reverse <$> go (reverse ns1', reverse ns2')
  where
    go = \case
      ([], ns2) -> pure ns2
      (ns1, []) -> pure ns1
      (n1 : ns1, 1 : ns2) -> (n1 :) <$> broadcast ns1 ns2
      (1 : ns1, n2 : ns2) -> (n2 :) <$> broadcast ns1 ns2
      (n1 : ns1, n2 : ns2) | n1 == n2 -> (n1 :) <$> broadcast ns1 ns2
      _ -> Nothing

arithmetic :: (Int -> Int -> Ass0Val) -> Ass0Val -> Ass0Val -> M Ass0Val
arithmetic f a0v1 a0v2 = do
  n1 <- validateIntLiteral a0v1
  n2 <- validateIntLiteral a0v2
  pure (f n1 n2)

logical :: (Bool -> Bool -> Ass0Val) -> Ass0Val -> Ass0Val -> M Ass0Val
logical f a0v1 a0v2 = do
  b1 <- validateBoolLiteral a0v1
  b2 <- validateBoolLiteral a0v2
  pure (f b1 b2)

$(deriveDeltaReduction definitions)

{-
reduceDeltaArity5 :: BuiltInArity5 -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> M Ass0Val
reduceDeltaArity5 bi5 a0v1 a0v2 a0v3 a0v4 a0v5 =
  case bi5 of
    BIDatasetHelperGenTrainBatch -> do
      ntrain <- validateIntLiteral a0v1
      ntest <- validateIntLiteral a0v2
      imgdim <- validateIntListLiteral a0v3
      labeldim <- validateIntListLiteral a0v4
      batchSize <- validateIntLiteral a0v5
      pure $ A0ValBracket (A1ValConst (A1BIDatasetHelperTrainBatch ntrain ntest imgdim labeldim batchSize))

reduceDeltaArity7 :: BuiltInArity7 -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> M Ass0Val
reduceDeltaArity7 bi7 a0v1 a0v2 a0v3 a0v4 a0v5 a0v6 a0v7 =
  case bi7 of
    BIDatasetHelperGenBatchAccuracy -> do
      ntrain <- validateIntLiteral a0v1
      ntest <- validateIntLiteral a0v2
      imgdim <- validateIntListLiteral a0v3
      labeldim <- validateIntListLiteral a0v4
      n <- validateIntLiteral a0v5
      batchSize <- validateIntLiteral a0v6
      let _f = a0v7
      pure $ A0ValBracket (A1ValConst (A1BIDatasetHelperBatchAccuracy ntrain ntest imgdim labeldim n batchSize))
    BITensorGenMaxPool2d -> do
      k <- validateIntLiteral a0v1
      l <- validateIntLiteral a0v2
      m <- validateIntLiteral a0v3
      n <- validateIntLiteral a0v4
      (padding1, padding2) <- validateIntPairLiteral a0v5
      (ksize1, ksize2) <- validateIntPairLiteral a0v6
      (stride1, stride2) <- validateIntPairLiteral a0v7
      pure $ A0ValBracket (A1ValConst (A1BITensorMaxPool2d k l m n padding1 padding2 ksize1 ksize2 stride1 stride2))
-}

reduceDeltaArity8 :: BuiltInArity8 -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> Ass0Val -> M Ass0Val
reduceDeltaArity8 bi8 a0v1 a0v2 a0v3 a0v4 a0v5 a0v6 a0v7 a0v8 =
  case bi8 of
    BILayerGenConv2d -> do
      l <- validateIntLiteral a0v1
      m <- validateIntLiteral a0v2
      n <- validateIntLiteral a0v3
      ksize <- validateIntLiteral a0v4
      stride <- validateIntLiteral a0v5
      padding <- validateIntLiteral a0v6
      input_dim <- validateIntLiteral a0v7
      output_dim <- validateIntLiteral a0v8
      pure $ A0ValBracket (A1ValConst (A1BILayerConv2d l m n ksize stride padding input_dim output_dim))

reduceDelta :: Ass0PartialBuiltInApp Ass0Val -> Ass0Val -> M Ass0Val
reduceDelta pba a0vArg =
  case pba of
    A0PartialBuiltInAppArity1 pba1 ->
      go pba1 a0vArg
    A0PartialBuiltInAppArity2 pba2 ->
      partial $ A0PartialBuiltInAppArity1 (PartialBuiltInAppArity1Cons pba2 a0vArg)
    A0PartialBuiltInAppArity3 pba3 ->
      partial $ A0PartialBuiltInAppArity2 (PartialBuiltInAppArity2Cons pba3 a0vArg)
    A0PartialBuiltInAppArity4 pba4 ->
      partial $ A0PartialBuiltInAppArity3 (PartialBuiltInAppArity3Cons pba4 a0vArg)
    A0PartialBuiltInAppArity5 pba5 ->
      partial $ A0PartialBuiltInAppArity4 (PartialBuiltInAppArity4Cons pba5 a0vArg)
    A0PartialBuiltInAppArity6 pba6 ->
      partial $ A0PartialBuiltInAppArity5 (PartialBuiltInAppArity5Cons pba6 a0vArg)
    A0PartialBuiltInAppArity7 pba7 ->
      partial $ A0PartialBuiltInAppArity6 (PartialBuiltInAppArity6Cons pba7 a0vArg)
    A0PartialBuiltInAppArity8 pba8 ->
      partial $ A0PartialBuiltInAppArity7 (PartialBuiltInAppArity7Cons pba8 a0vArg)
  where
    partial = pure . A0ValPartialBuiltInApp

    go pba1 v1 =
      case pba1 of
        PartialBuiltInAppArity1Nil bi1 ->
          reduceDeltaArity1 bi1 v1
        PartialBuiltInAppArity1Cons pba2 v2 ->
          case pba2 of
            PartialBuiltInAppArity2Nil bi2 ->
              reduceDeltaArity2 bi2 v2 v1
            PartialBuiltInAppArity2Cons pba3 v3 ->
              case pba3 of
                PartialBuiltInAppArity3Nil bi3 ->
                  reduceDeltaArity3 bi3 v3 v2 v1
                PartialBuiltInAppArity3Cons pba4 v4 ->
                  case pba4 of
                    PartialBuiltInAppArity4Cons pba5 v5 ->
                      case pba5 of
                        PartialBuiltInAppArity5Nil bi5 ->
                          reduceDeltaArity5 bi5 v5 v4 v3 v2 v1
                        PartialBuiltInAppArity5Cons pba6 v6 ->
                          case pba6 of
                            PartialBuiltInAppArity6Cons pba7 v7 ->
                              case pba7 of
                                PartialBuiltInAppArity7Nil bi7 ->
                                  reduceDeltaArity7 bi7 v7 v6 v5 v4 v3 v2 v1
                                PartialBuiltInAppArity7Cons pba8 v8 ->
                                  case pba8 of
                                    PartialBuiltInAppArity8Nil bi8 ->
                                      reduceDeltaArity8 bi8 v8 v7 v6 v5 v4 v3 v2 v1

reduceBeta :: Ass0Val -> Ass0Val -> M Ass0Val
reduceBeta a0vFun a0vArg =
  case a0vFun of
    A0ValLam Nothing (x, _a0tyv) a0eBody env ->
      evalExpr0
        (Map.insert x (Ass0ValEntry a0vArg) env)
        a0eBody
    A0ValLam (Just (f, _a0tyvRec)) (x, _a0tyv) a0eBody env ->
      evalExpr0
        (Map.insert x (Ass0ValEntry a0vArg) (Map.insert f (Ass0ValEntry a0vFun) env))
        a0eBody
    A0ValPartialBuiltInApp pba ->
      reduceDelta pba a0vArg
    _ ->
      bug $ NotAClosure a0vFun

-- TODO (enhance): fix this to handle polymorphism properly
reduceTypeBeta0 :: Ass0Val -> Ass0TypeVal -> M Ass0Val
reduceTypeBeta0 a0vTypeFun _a0tyvArg =
  pure a0vTypeFun

-- TODO (enhance): fix this to handle polymorphism properly
reduceTypeBeta1 :: Ass1Val -> Ass1TypeVal -> M Ass1Val
reduceTypeBeta1 a1vTypeFun _a1tyvArg =
  pure a1vTypeFun

evalExpr0 :: EvalEnv -> Ass0Expr -> M Ass0Val
evalExpr0 env = \case
  A0Literal lit ->
    A0ValLiteral <$> mapMAssLiteral (evalExpr0 env) lit
  A0Var x ->
    findVal0 env x
  A0BuiltInName bi ->
    pure $
      case bi of
        BuiltInArity1 bi1 -> A0ValPartialBuiltInApp (A0PartialBuiltInAppArity1 (PartialBuiltInAppArity1Nil bi1))
        BuiltInArity2 bi2 -> A0ValPartialBuiltInApp (A0PartialBuiltInAppArity2 (PartialBuiltInAppArity2Nil bi2))
        BuiltInArity3 bi3 -> A0ValPartialBuiltInApp (A0PartialBuiltInAppArity3 (PartialBuiltInAppArity3Nil bi3))
        BuiltInArity5 bi5 -> A0ValPartialBuiltInApp (A0PartialBuiltInAppArity5 (PartialBuiltInAppArity5Nil bi5))
        BuiltInArity7 bi7 -> A0ValPartialBuiltInApp (A0PartialBuiltInAppArity7 (PartialBuiltInAppArity7Nil bi7))
        BuiltInArity8 bi8 -> A0ValPartialBuiltInApp (A0PartialBuiltInAppArity8 (PartialBuiltInAppArity8Nil bi8))
        BuiltInOther s -> error $ "BuiltInOther: " ++ Text.unpack s
  A0Lam Nothing (x, a0tye1) a0e2 -> do
    a0tyv1 <- evalTypeExpr0 env a0tye1
    pure $ A0ValLam Nothing (x, a0tyv1) a0e2 env
  A0Lam (Just (f, a0tyeRec)) (x, a0tye1) a0e2 -> do
    a0tyvRec <- evalTypeExpr0 env a0tyeRec
    a0tyv1 <- evalTypeExpr0 env a0tye1
    pure $ A0ValLam (Just (f, a0tyvRec)) (x, a0tyv1) a0e2 env
  A0App a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    a0v2 <- evalExpr0 env a0e2
    reduceBeta a0v1 a0v2
  A0LetIn (x, a0tye1) a0e1 a0e2 ->
    evalExpr0 env (A0App (A0Lam Nothing (x, a0tye1) a0e2) a0e1)
  A0LetTupleIn xL xR a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    case a0v1 of
      A0ValTuple a0vL a0vR ->
        evalExpr0
          (env & Map.insert xL (Ass0ValEntry a0vL) & Map.insert xR (Ass0ValEntry a0vR))
          a0e2
      _ ->
        bug $ NotATuple a0v1
  A0Sequential a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    () <- validateUnitLiteral a0v1
    evalExpr0 env a0e2
  A0Tuple a0e1 a0e2 -> do
    a0v1 <- evalExpr0 env a0e1
    a0v2 <- evalExpr0 env a0e2
    pure $ A0ValTuple a0v1 a0v2
  A0IfThenElse a0e0 a0e1 a0e2 -> do
    a0v0 <- evalExpr0 env a0e0
    b <- validateBoolLiteral a0v0
    if b
      then evalExpr0 env a0e1
      else evalExpr0 env a0e2
  A0Bracket a1e1 -> do
    a1v1 <- evalExpr1 env a1e1
    pure $ A0ValBracket a1v1
  A0TyEqAssert loc ty1eq -> do
    let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    if a1tyv1 == a1tyv2 -- We can use `==` for stage-1 types
      then
        generateIdentityFunction env (A0TyValCode a1tyv1)
      else do
        EvalState {sourceSpec} <- get
        let spanInFile = getSpanInFile sourceSpec loc
        evalError $ AssertionFailure spanInFile a1tyv1 a1tyv2
  A0RefinementAssert loc a0ePred a0eTarget -> do
    a0vPred <- evalExpr0 env a0ePred
    a0vTarget <- evalExpr0 env a0eTarget
    b <- validateBoolLiteral =<< reduceBeta a0vPred a0vTarget
    if b
      then
        pure a0vTarget
      else do
        EvalState {sourceSpec} <- get
        let spanInFile = getSpanInFile sourceSpec loc
        evalError $ RefinementAssertionFailure spanInFile a0vPred a0vTarget
  A0AppType a0e1 sa0tye2 -> do
    a0v1 <- evalExpr0 env a0e1
    a0tyv2 <- evalTypeExpr0 env sa0tye2
    reduceTypeBeta0 a0v1 a0tyv2

evalExpr1 :: EvalEnv -> Ass1Expr -> M Ass1Val
evalExpr1 env = \case
  A1Literal lit ->
    A1ValLiteral <$> mapMAssLiteral (evalExpr1 env) lit
  A1Var x -> do
    symb <- findSymbol env x
    pure $ A1ValVar symb
  A1BuiltInName a1bi ->
    pure $ A1ValConst a1bi
  A1Lam Nothing (x, a1tye1) a1e2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    symbX <- generateFreshSymbol
    a1v2 <- evalExpr1 (Map.insert x (SymbolEntry symbX) env) a1e2
    pure $ A1ValLam Nothing (symbX, a1tyv1) a1v2
  A1Lam (Just (f, a1tyeRec)) (x, a1tye1) a1e2 -> do
    a1tyvRec <- evalTypeExpr1 env a1tyeRec
    a1tyv1 <- evalTypeExpr1 env a1tye1
    symbF <- generateFreshSymbol
    symbX <- generateFreshSymbol
    a1v1 <- evalExpr1 (Map.insert x (SymbolEntry symbX) (Map.insert f (SymbolEntry symbF) env)) a1e2
    pure $ A1ValLam (Just (symbF, a1tyvRec)) (symbX, a1tyv1) a1v1
  A1App a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValApp a1v1 a1v2
  A1LetTupleIn xL xR a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    symbXL <- generateFreshSymbol
    symbXR <- generateFreshSymbol
    a1v2 <- evalExpr1 (env & Map.insert xL (SymbolEntry symbXL) & Map.insert xR (SymbolEntry symbXR)) a1e2
    pure $ A1ValLetTupleIn symbXL symbXR a1v1 a1v2
  A1Sequential a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValSequential a1v1 a1v2
  A1Tuple a1e1 a1e2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValTuple a1v1 a1v2
  A1IfThenElse a1e0 a1e1 a1e2 -> do
    a1v0 <- evalExpr1 env a1e0
    a1v1 <- evalExpr1 env a1e1
    a1v2 <- evalExpr1 env a1e2
    pure $ A1ValIfThenElse a1v0 a1v1 a1v2
  A1Escape a0e1 -> do
    a0v1 <- evalExpr0 env a0e1
    case a0v1 of
      A0ValBracket a1v1 -> pure a1v1
      _ -> bug $ NotACodeValue a0v1
  A1AppType a1e1 a1tye2 -> do
    a1v1 <- evalExpr1 env a1e1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    reduceTypeBeta1 a1v1 a1tyv2

evalTypeExpr0 :: EvalEnv -> StrictAss0TypeExpr -> M Ass0TypeVal
evalTypeExpr0 env = \case
  SA0TyPrim a0tyPrim maybePred -> do
    let a0tyValPrim =
          case a0tyPrim of
            A0TyPrimBase tyPrimBase -> A0TyValPrimBase tyPrimBase
            A0TyTensor n -> A0TyValTensor n
            A0TyDataset dsParam -> A0TyValDataset dsParam
    maybeVPred <- mapM (evalExpr0 env) maybePred
    pure $ A0TyValPrim a0tyValPrim maybeVPred
  SA0TyVar atyvar ->
    pure $ A0TyValVar atyvar
  SA0TyList sa0tye1 maybePred -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    maybeVPred <- mapM (evalExpr0 env) maybePred
    pure $ A0TyValList a0tyv1 maybeVPred
  SA0TyProduct sa0tye1 sa0tye2 -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    a0tyv2 <- evalTypeExpr0 env sa0tye2
    pure $ A0TyValProduct a0tyv1 a0tyv2
  SA0TyArrow (xOpt, sa0tye1) sa0tye2 -> do
    a0tyv1 <- evalTypeExpr0 env sa0tye1
    pure $ A0TyValArrow (xOpt, a0tyv1) sa0tye2
  SA0TyCode a1tye1 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    pure $ A0TyValCode a1tyv1
  SA0TyExplicitForAll atyvar sa0tye1 -> do
    pure $ A0TyValExplicitForAll atyvar sa0tye1

evalTypeExpr1 :: EvalEnv -> Ass1TypeExpr -> M Ass1TypeVal
evalTypeExpr1 env = \case
  A1TyPrim a1tyPrim ->
    A1TyValPrim
      <$> case a1tyPrim of
        A1TyPrimBase tyPrimBase ->
          pure $ A1TyValPrimBase tyPrimBase
        A1TyTensor a0eList -> do
          a0v <- evalExpr0 env a0eList
          a0vs <- validateListValue a0v
          ns <- mapM validateIntLiteral a0vs
          pure $ A1TyValTensor ns
        A1TyDataset datasetParam -> do
          numTrain <- validateIntLiteral =<< evalExpr0 env datasetParam.numTrain
          numTest <- validateIntLiteral =<< evalExpr0 env datasetParam.numTest
          image <- validateIntListLiteral =<< evalExpr0 env (runIdentity datasetParam.image)
          label <- validateIntListLiteral =<< evalExpr0 env (runIdentity datasetParam.label)
          pure $ A1TyValDataset DatasetParam {numTrain, numTest, image, label}
  A1TyList a1tye -> do
    a1tyv <- evalTypeExpr1 env a1tye
    pure $ A1TyValList a1tyv
  A1TyVar atyvar ->
    pure $ A1TyValVar atyvar
  A1TyProduct a1tye1 a1tye2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    pure $ A1TyValProduct a1tyv1 a1tyv2
  A1TyArrow labelOpt a1tye1 a1tye2 -> do
    a1tyv1 <- evalTypeExpr1 env a1tye1
    a1tyv2 <- evalTypeExpr1 env a1tye2
    pure $ A1TyValArrow labelOpt a1tyv1 a1tyv2
  A1TyImplicitForAll atyvar a1tye2 -> do
    pure $ A1TyValImplicitForAll atyvar a1tye2

run :: M a -> EvalState -> Either EvalError a
run = evalStateT

unliftVal :: Ass1Val -> Ass0Expr
unliftVal = \case
  A1ValLiteral lit ->
    A0Literal (mapAssLiteral unliftVal lit)
  A1ValConst a1bi ->
    A0BuiltInName (unliftBuiltInName a1bi)
  A1ValVar symbX ->
    A0Var (symbolToVar symbX)
  A1ValLam Nothing (symbX, a1tyv1) a1v2 ->
    A0Lam Nothing (symbolToVar symbX, unliftTypeVal a1tyv1) (unliftVal a1v2)
  A1ValLam (Just (symbF, a1tyvRec)) (symbX, a1tyv1) a1v2 ->
    A0Lam (Just (symbolToVar symbF, unliftTypeVal a1tyvRec)) (symbolToVar symbX, unliftTypeVal a1tyv1) (unliftVal a1v2)
  A1ValApp a1v1 a1v2 ->
    A0App (unliftVal a1v1) (unliftVal a1v2)
  A1ValLetTupleIn symbXL symbXR a1v1 a1v2 ->
    A0LetTupleIn (symbolToVar symbXL) (symbolToVar symbXR) (unliftVal a1v1) (unliftVal a1v2)
  A1ValSequential a1v1 a1v2 ->
    A0Sequential (unliftVal a1v1) (unliftVal a1v2)
  A1ValTuple a1v1 a1v2 ->
    A0Tuple (unliftVal a1v1) (unliftVal a1v2)
  A1ValIfThenElse a1v0 a1v1 a1v2 ->
    A0IfThenElse (unliftVal a1v0) (unliftVal a1v1) (unliftVal a1v2)

unliftTypeVal :: Ass1TypeVal -> StrictAss0TypeExpr
unliftTypeVal = \case
  A1TyValPrim a1tyvPrim ->
    let a0tyPrim =
          case a1tyvPrim of
            A1TyValPrimBase tyPrimBase -> A0TyPrimBase tyPrimBase
            A1TyValTensor ns -> A0TyTensor ns
            A1TyValDataset datasetParam -> A0TyDataset datasetParam
     in SA0TyPrim a0tyPrim Nothing
  A1TyValList a1tyv ->
    SA0TyList (unliftTypeVal a1tyv) Nothing
  A1TyValVar _atyvar ->
    error "TODO: unliftTypeVal, A1TyValVar"
  A1TyValProduct a1tyv1 a1tyv2 ->
    SA0TyProduct (unliftTypeVal a1tyv1) (unliftTypeVal a1tyv2)
  A1TyValArrow _labelOpt a1tyv1 a1tyv2 ->
    SA0TyArrow (Nothing, unliftTypeVal a1tyv1) (unliftTypeVal a1tyv2)
  A1TyValImplicitForAll _atyvar _a1tye2 ->
    error "TODO: unliftTypeVal, A1TyValImplicitForAll"
