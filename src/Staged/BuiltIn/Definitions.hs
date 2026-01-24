{-# LANGUAGE TemplateHaskell #-}

module Staged.BuiltIn.Definitions
  ( definitions,
  )
where

import Data.List (intercalate)
import Data.List qualified as List
import Language.Haskell.TH qualified as TH
import Staged.BuiltIn.CompileTime
import Util.Matrix qualified as Matrix
import Util.String (snakeToCamel, uppercase)
import Util.Vector qualified as Vector
import Prelude

gen :: [String] -> String -> [ParamSpec] -> BuiltInSpec
gen modules name params =
  BuiltInSpec
    { common = Common {constructor0, constructorDisplay0},
      main = Gen genSpec
    }
  where
    genSpec = GenSpec {name0, params, constructor1, constructorDisplay1}
    constructor0 = "BI" ++ concat (map snakeToCamel modules ++ ["Gen" ++ snakeToCamel name])
    constructorDisplay0 = intercalate "." $ map uppercase (modules ++ [name])
    constructor1 = "A1BI" ++ concatMap snakeToCamel (modules ++ [name])
    constructorDisplay1 = intercalate "." $ map snakeToCamel modules ++ [name]
    name0 = intercalate "__" $ modules ++ ["gen_" ++ name]

data Availability
  = ForStage0
  | ForStage1
  | ForBothStages
  | ForInternal [ParamSpec]

versatile :: [String] -> String -> Availability -> Int -> TH.Q TH.Exp -> BuiltInSpec
versatile modules name availability arity bodyQ =
  BuiltInSpec
    { common = Common {constructor0, constructorDisplay0},
      main = Versatile versSpec
    }
  where
    versSpec = VersatileSpec {name0, nameAndConstructor1, fixedParams, arity, bodyQ}
    constructorSuffix = concatMap snakeToCamel (modules ++ [name])
    constructor0 = "BI" ++ constructorSuffix
    constructorDisplay0 = intercalate "." $ map uppercase (modules ++ [name])
    name0 =
      case availability of
        ForInternal _ -> Nothing
        ForStage1 -> Nothing
        _ -> Just $ intercalate "__" $ modules ++ [name]
    nameAndConstructor1 =
      case availability of
        ForInternal _ -> Nothing
        ForStage0 -> Nothing
        _ -> Just (intercalate "__" (modules ++ [name]), "A1BI" ++ constructorSuffix)
    fixedParams =
      case availability of
        ForInternal ps -> ps
        _ -> []

definitions :: [BuiltInSpec]
definitions =
  [ versatile [] "int_add" ForBothStages 2 $
      [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 + n2))) a0v1 a0v2|],
    versatile [] "int_sub" ForBothStages 2 $
      [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 - n2))) a0v1 a0v2|],
    versatile [] "int_mult" ForBothStages 2 $
      [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 * n2))) a0v1 a0v2|],
    versatile [] "int_div" ForBothStages 2 $
      [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 `div` n2))) a0v1 a0v2|],
    versatile [] "float_div" ForBothStages 2 $
      [|
        do
          r1 <- validateFloatLiteral a0v1
          r2 <- validateFloatLiteral a0v2
          pure $ A0ValLiteral (ALitFloat (r1 / r2))
        |],
    versatile [] "int_mod" ForBothStages 2 $
      [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 `mod` n2))) a0v1 a0v2|],
    versatile [] "int_leq" ForBothStages 2 $
      [|arithmetic (\n1 n2 -> A0ValLiteral (ALitBool (n1 <= n2))) a0v1 a0v2|],
    versatile [] "int_equal" ForBothStages 2 $
      [|arithmetic (\n1 n2 -> A0ValLiteral (ALitBool (n1 == n2))) a0v1 a0v2|],
    versatile [] "and" ForBothStages 2 $
      [|logical (\b1 b2 -> A0ValLiteral (ALitBool (b1 && b2))) a0v1 a0v2|],
    versatile ["list"] "cons" ForBothStages 2 $
      [|
        do
          a0vs2 <- validateListValue a0v2
          pure $ A0ValLiteral (ALitList (a0v1 : a0vs2))
        |],
    gen [] "vadd" [ParamInt],
    versatile [] "vadd" (ForInternal [ParamInt]) 2 $
      [|
        do
          v1 <- validateVec0 a0v1
          v2 <- validateVec0 a0v2
          case Vector.add p1 v1 v2 of
            Just v -> pure $ A0ValLiteral (ALitVec v)
            Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
        |],
    gen [] "vconcat" [ParamInt, ParamInt],
    versatile [] "vconcat" (ForInternal [ParamInt, ParamInt]) 2 $
      [|
        do
          v1 <- validateVec0 a0v1
          v2 <- validateVec0 a0v2
          case Vector.concat p1 p2 v1 v2 of
            Just v -> pure $ A0ValLiteral (ALitVec v)
            Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
        |],
    gen [] "mtranspose" [ParamInt, ParamInt],
    versatile [] "mtranspose" (ForInternal [ParamInt, ParamInt]) 1 $
      [|
        do
          mat1 <- validateMat0 a0v1
          case Matrix.transpose p1 p2 mat1 of
            Just mat -> pure $ A0ValLiteral (ALitMat mat)
            Nothing -> bug $ InconsistentAppBuiltInArity1 bi1 a0v1
        |],
    gen [] "mconcat_vert" [ParamInt, ParamInt, ParamInt],
    versatile [] "mconcat_vert" (ForInternal [ParamInt, ParamInt, ParamInt]) 2 $
      [|
        do
          mat1 <- validateMat0 a0v1
          mat2 <- validateMat0 a0v2
          case Matrix.concatVert p1 p2 p3 mat1 mat2 of
            Just mat -> pure $ A0ValLiteral (ALitMat mat)
            Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
        |],
    versatile [] "drop_at" ForStage0 2 $
      [|
        do
          n1 <- validateIntLiteral a0v1
          a0vs2 <- validateListValue a0v2
          pure $ A0ValLiteral (ALitList (dropAt n1 a0vs2))
        |],
    versatile [] "broadcastable" ForStage0 2 $
      [|
        do
          ns1 <- validateIntListLiteral a0v1
          ns2 <- validateIntListLiteral a0v2
          let b = isJust (broadcast ns1 ns2)
          pure $ A0ValLiteral (ALitBool b)
        |],
    versatile [] "broadcast" ForStage0 2 $
      [|
        do
          ns1 <- validateIntListLiteral a0v1
          ns2 <- validateIntListLiteral a0v2
          ns <-
            case broadcast ns1 ns2 of
              Just ns' -> pure ns'
              Nothing -> bug $ BroadcastFailed ns1 ns2
          pure $ A0ValLiteral (ALitList (map (A0ValLiteral . ALitInt) ns))
        |],
    versatile [] "reshapeable" ForStage0 2 $
      [|
        do
          ns1 <- validateIntListLiteral a0v1
          ns2 <- validateIntListLiteral a0v2
          let b = List.foldl' (*) 1 ns1 == List.foldl' (*) 1 ns2
          pure $ A0ValLiteral (ALitBool b)
        |],
    versatile [] "float" ForBothStages 1 $
      [|
        do
          n <- validateIntLiteral a0v1
          pure $ A0ValLiteral (ALitFloat (fromIntegral n))
        |],
    versatile [] "print_float" ForBothStages 1 $
      [|
        do
          _r <- validateFloatLiteral a0v1
          -- TODO: print `r` here
          pure $ A0ValLiteral ALitUnit
        |],
    versatile [] "print_string" ForBothStages 1 $
      [|
        do
          _s <- validateStringLiteral a0v1
          -- TODO: print `s` here
          pure $ A0ValLiteral ALitUnit
        |],
    versatile [] "range" ForBothStages 2 $
      [|
        do
          n1 <- validateIntLiteral a0v1
          n2 <- validateIntLiteral a0v2
          pure $ A0ValLiteral (ALitList (map (A0ValLiteral . ALitInt) [n1 .. n2]))
        |],
    versatile [] "lift_string" ForStage0 1 $
      [|
        do
          s <- validateStringLiteral a0v1
          pure $ A0ValBracket (A1ValLiteral (ALitString s))
        |],
    versatile [] "fst" ForBothStages 1 $
      [|
        do
          (a0v11, _) <- validateTupleValue a0v1
          pure a0v11
        |],
    versatile [] "snd" ForBothStages 1 $
      [|
        do
          (_, a0v12) <- validateTupleValue a0v1
          pure a0v12
        |],
    versatile ["list"] "map" ForBothStages 2 $
      [|
        do
          a0vsIn <- validateListValue a0v2
          a0vsOut <- mapM (reduceBeta a0v1) a0vsIn
          pure $ A0ValLiteral (ALitList a0vsOut)
        |],
    versatile ["list"] "append" ForBothStages 2 $
      [|
        do
          a0vs1 <- validateListValue a0v1
          a0vs2 <- validateListValue a0v2
          pure $ A0ValLiteral (ALitList (a0vs1 ++ a0vs2))
        |],
    versatile ["list"] "iter" ForBothStages 2 $
      [|
        do
          a0vsIn <- validateListValue a0v2
          forM_ a0vsIn (reduceBeta a0v1 >=> validateUnitLiteral)
          pure $ A0ValLiteral ALitUnit
        |],
    versatile ["device"] "cpu" ForStage1 0 $
      [|error "TODO: Device.cpu"|],
    versatile ["device"] "gen_cuda_if_available" ForStage0 1 $
      [|
        do
          () <- validateUnitLiteral a0v1
          pure $ A0ValBracket (A1ValLiteral ALitUnit) -- TODO: return a value of type `Device`
        |],
    -- TODO: built-in functions should be reordered henceforth:
    gen ["tensor"] "zeros" [ParamIntList],
    gen ["tensor"] "grad" [ParamIntList],
    gen ["tensor"] "zero_grad" [ParamIntList],
    gen ["tensor"] "sub_update" [ParamIntList],
    gen ["tensor"] "count_equal" [ParamIntList],
    gen ["tensor"] "dropout" [ParamIntList],
    versatile
      ["tensor"]
      "f"
      ForStage1
      1
      [|
        do
          _r <- validateFloatLiteral a0v1
          error "TODO: Tensor.f"
        |],
    versatile
      ["tensor"]
      "float_value"
      ForStage1
      1
      [|
        do
          error "TODO: Tensor.float_value"
        |],
    versatile
      ["tensor"]
      "backward"
      ForStage1
      1
      [|
        do
          let _tensor = a0v1
          error "TODO: Tensor.backward"
        |],
    versatile
      ["tensor"]
      "no_grad"
      ForStage1
      1
      [|
        do
          let _f = a0v1
          error "TODO: Tensor.no_grad"
        |],
    versatile
      ["layer", "activation"]
      "relu"
      ForStage1
      0
      [|error "TODO: Layer.Activation.relu"|],
    versatile
      ["var_store"]
      "create"
      ForStage1
      4
      [|
        do
          _frozen <- validateBoolLiteral a0v1
          _name <- validateStringLiteral a0v2
          let _device = a0v3
          () <- validateUnitLiteral a0v4
          error "TODO: VarStore.create"
        |],
    versatile
      ["optimizer"]
      "adam"
      ForStage1
      2
      [|
        do
          let _varStore = a0v1
          _r <- validateFloatLiteral a0v2
          error "TODO: Optimizer.adam"
        |],
    versatile
      ["optimizer"]
      "backward_step"
      ForStage1
      2
      [|
        do
          let _optimizer = a0v1
          let _tensor = a0v2
          error "TODO: Optimizer.backward_step"
        |],
    versatile
      ["layer", "activation"]
      "none"
      ForStage1
      0
      [|error "TODO: Layer.Activation.none"|],
    versatile
      ["mnist_helper"]
      "train_images"
      ForStage1
      0
      [|error "TODO: MnistHelper.train_images"|],
    versatile
      ["mnist_helper"]
      "train_labels"
      ForStage1
      0
      [|error "TODO: MnistHelper.train_labels"|],
    versatile
      ["mnist_helper"]
      "test_images"
      ForStage1
      0
      [|error "TODO: MnistHelper.test_images"|],
    versatile
      ["mnist_helper"]
      "test_labels"
      ForStage1
      0
      [|error "TODO: MnistHelper.test_labels"|],
    gen ["tensor"] "add" [ParamIntList, ParamIntList],
    gen ["tensor"] "mult" [ParamIntList, ParamIntList],
    gen ["tensor"] "argmax" [ParamIntList, ParamInt],
    gen ["tensor"] "cross_entropy_for_logits" [ParamInt, ParamInt],
    versatile
      ["tensor"]
      "add"
      (ForInternal [ParamIntList])
      2
      [|
        case p1 of
          [n] -> do
            v1 <- validateVec0 a0v1
            v2 <- validateVec0 a0v2
            case Vector.add n v1 v2 of
              Just v -> pure $ A0ValLiteral (ALitVec v)
              Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
          [m, n] -> do
            mat1 <- validateMat0 a0v1
            mat2 <- validateMat0 a0v2
            case Matrix.add m n mat1 mat2 of
              Just mat -> pure $ A0ValLiteral (ALitMat mat)
              Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
          _ ->
            error "TODO: evalExpr0, BITadd, dimension >= 3"
        |],
    versatile
      ["tensor"]
      "mm"
      (ForInternal [ParamInt, ParamInt, ParamInt])
      2
      [|
        do
          mat1 <- validateMat0 a0v1
          mat2 <- validateMat0 a0v2
          case Matrix.mult p1 p2 p3 mat1 mat2 of
            Just mat -> pure $ A0ValLiteral (ALitMat mat)
            Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
        |],
    gen ["tensor"] "reshape" [ParamIntList, ParamIntList],
    gen ["layer"] "forward" [ParamIntList, ParamIntList],
    gen ["tensor"] "mm" [ParamInt, ParamInt, ParamInt],
    gen ["layer"] "linear" [ParamIntList, ParamInt, ParamInt],
    -- Arity 5:
    gen ["dataset_helper"] "train_batch" [ParamInt, ParamInt, ParamIntList, ParamIntList, ParamInt],
    -- Arity 7:
    gen ["dataset_helper"] "batch_accuracy" [ParamInt, ParamInt, ParamIntList, ParamIntList, ParamInt, ParamInt, ParamDiscarded],
    gen ["tensor"] "max_pool2d" [ParamInt, ParamInt, ParamInt, ParamInt, ParamIntPair, ParamIntPair, ParamIntPair],
    -- Arity 8:
    gen ["layer"] "conv2d_" [ParamInt, ParamInt, ParamInt, ParamInt, ParamInt, ParamInt, ParamInt, ParamInt]
  ]
