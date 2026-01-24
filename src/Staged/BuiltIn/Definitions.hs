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

data Availability = ForStage0 | ForStage1 | ForBothStages

versatile :: [String] -> String -> Availability -> [ParamSpec] -> Int -> TH.Q TH.Exp -> BuiltInSpec
versatile modules name availability fixedParams arity bodyQ =
  BuiltInSpec
    { common = Common {constructor0, constructorDisplay0},
      main = Versatile versSpec
    }
  where
    versSpec = VersatileSpec {name0, fixedParams, arity, bodyQ}
    constructor0 = "BI" ++ concatMap snakeToCamel (modules ++ [name])
    constructorDisplay0 = intercalate "." $ map uppercase (modules ++ [name])
    name0 =
      case availability of
        ForStage1 -> Nothing
        _ -> Just $ intercalate "__" $ modules ++ [name]

definitions :: [BuiltInSpec]
definitions =
  [ -- Arity 1:
    gen [] "vadd" [ParamInt],
    gen ["tensor"] "zeros" [ParamIntList],
    gen ["tensor"] "grad" [ParamIntList],
    gen ["tensor"] "zero_grad" [ParamIntList],
    gen ["tensor"] "sub_update" [ParamIntList],
    gen ["tensor"] "count_equal" [ParamIntList],
    gen ["tensor"] "dropout" [ParamIntList],
    versatile [] "fst" ForBothStages [] 1
        [|
          do
            (a0v11, _) <- validateTupleValue a0v1
            pure a0v11
         |],
    versatile [] "snd" ForBothStages [] 1
        [|
          do
            (_, a0v12) <- validateTupleValue a0v1
            pure a0v12
          |],
    versatile ["device"] "gen_cuda_if_available" ForStage0
        []
        1
        [|
          do
            () <- validateUnitLiteral a0v1
            pure $ A0ValBracket (A1ValLiteral ALitUnit) -- TODO: return a value of type `Device`
          |],
    versatile [] "mtranspose"
        ForStage1
        [ParamInt, ParamInt]
        1
        [|
          do
            mat1 <- validateMat0 a0v1
            case Matrix.transpose p1 p2 mat1 of
              Just mat -> pure $ A0ValLiteral (ALitMat mat)
              Nothing -> bug $ InconsistentAppBuiltInArity1 bi1 a0v1
          |],
    -- Arity 2:
    versatile [] "add" ForBothStages [] 2
        [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 + n2))) a0v1 a0v2|],
    versatile [] "sub" ForBothStages [] 2
        [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 - n2))) a0v1 a0v2|],
    versatile [] "mult" ForBothStages [] 2
        [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 * n2))) a0v1 a0v2|],
    versatile [] "div" ForBothStages [] 2
        [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 `div` n2))) a0v1 a0v2|],
    versatile [] "mod" ForBothStages [] 2
        [|arithmetic (\n1 n2 -> A0ValLiteral (ALitInt (n1 `mod` n2))) a0v1 a0v2|],
    versatile [] "leq" ForBothStages [] 2
        [|arithmetic (\n1 n2 -> A0ValLiteral (ALitBool (n1 <= n2))) a0v1 a0v2|],
    versatile [] "equal" ForBothStages [] 2
        [|arithmetic (\n1 n2 -> A0ValLiteral (ALitBool (n1 == n2))) a0v1 a0v2|],
    versatile [] "and" ForBothStages [] 2
        [|logical (\b1 b2 -> A0ValLiteral (ALitBool (b1 && b2))) a0v1 a0v2|],
    versatile ["list"] "map" ForBothStages [] 2
        [|
          do
            a0vsIn <- validateListValue a0v2
            a0vsOut <- mapM (reduceBeta a0v1) a0vsIn
            pure $ A0ValLiteral (ALitList a0vsOut)
          |],
    gen [] "vconcat" [ParamInt, ParamInt],
    gen [] "mtranspose" [ParamInt, ParamInt],
    gen ["tensor"] "add" [ParamIntList, ParamIntList],
    versatile [] "vadd" ForStage1 [ParamInt] 2
        [|
          do
            v1 <- validateVec0 a0v1
            v2 <- validateVec0 a0v2
            case Vector.add p1 v1 v2 of
              Just v -> pure $ A0ValLiteral (ALitVec v)
              Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
          |],
    versatile [] "vconcat" ForStage1 [ParamInt, ParamInt] 2
        [|
          do
            v1 <- validateVec0 a0v1
            v2 <- validateVec0 a0v2
            case Vector.concat p1 p2 v1 v2 of
              Just v -> pure $ A0ValLiteral (ALitVec v)
              Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
          |],
    versatile [] "mconcat_vert" ForStage1 [ParamInt, ParamInt, ParamInt] 2
        [|
          do
            mat1 <- validateMat0 a0v1
            mat2 <- validateMat0 a0v2
            case Matrix.concatVert p1 p2 p3 mat1 mat2 of
              Just mat -> pure $ A0ValLiteral (ALitMat mat)
              Nothing -> bug $ InconsistentAppBuiltInArity2 bi2 a0v1 a0v2
          |],
    versatile [] "drop_at" ForStage0 [] 2
        [|
          do
            n1 <- validateIntLiteral a0v1
            a0vs2 <- validateListValue a0v2
            pure $ A0ValLiteral (ALitList (dropAt n1 a0vs2))
          |],
    versatile [] "broadcastable" ForStage0 [] 2
        [|
          do
            ns1 <- validateIntListLiteral a0v1
            ns2 <- validateIntListLiteral a0v2
            let b = isJust (broadcast ns1 ns2)
            pure $ A0ValLiteral (ALitBool b)
          |],
    versatile [] "broadcast" ForStage0 [] 2
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
    versatile [] "reshapeable" ForStage0 [] 2
        [|
          do
            ns1 <- validateIntListLiteral a0v1
            ns2 <- validateIntListLiteral a0v2
            let b = List.foldl' (*) 1 ns1 == List.foldl' (*) 1 ns2
            pure $ A0ValLiteral (ALitBool b)
          |],
    versatile ["list"] "cons" ForBothStages [] 2
        [|
          do
            a0vs2 <- validateListValue a0v2
            pure $ A0ValLiteral (ALitList (a0v1 : a0vs2))
          |],
    versatile ["list"] "append" ForBothStages [] 2
        [|
          do
            a0vs1 <- validateListValue a0v1
            a0vs2 <- validateListValue a0v2
            pure $ A0ValLiteral (ALitList (a0vs1 ++ a0vs2))
          |],
    versatile ["list"] "iter" ForBothStages [] 2
        [|
          do
            a0vsIn <- validateListValue a0v2
            forM_ a0vsIn (reduceBeta a0v1 >=> validateUnitLiteral)
            pure $ A0ValLiteral ALitUnit
          |],
    gen ["tensor"] "mult" [ParamIntList, ParamIntList],
    gen ["tensor"] "argmax" [ParamIntList, ParamInt],
    gen ["tensor"] "cross_entropy_for_logits" [ParamInt, ParamInt],
    versatile ["tensor"] "add" ForStage1 [ParamIntList] 2
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
    versatile ["tensor"] "mm" ForStage1 [ParamInt, ParamInt, ParamInt] 2
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
    -- Arity 3:
    gen [] "mconcat_vert" [ParamInt, ParamInt, ParamInt],
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
