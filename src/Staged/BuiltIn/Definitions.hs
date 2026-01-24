{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.Definitions
  ( definitions,
  )
where

import Data.List (intercalate)
import Staged.BuiltIn.CompileTime
import Util.Matrix qualified as Matrix
import Util.String (snakeToCamel, uppercase)
import Prelude

gen :: [String] -> String -> [ParamSpec] -> BuiltInSpec
gen modules name params =
  BuiltInSpec
    { common = Common {constructor0, constructorDisplay0},
      main = Gen genSpec
    }
  where
    constructor0 = "BI" ++ concat (map snakeToCamel modules ++ ["Gen" ++ snakeToCamel name])
    constructorDisplay0 = intercalate "." $ map uppercase (modules ++ [name])
    constructor1 = "A1BI" ++ concatMap snakeToCamel (modules ++ [name])
    constructorDisplay1 = intercalate "." $ map snakeToCamel modules ++ [name]
    genSpec = GenSpec {params, constructor1, constructorDisplay1}

versatile :: String -> String -> VersatileSpec -> BuiltInSpec
versatile constructor0 constructorDisplay0 versSpec =
  BuiltInSpec
    { common = Common {constructor0, constructorDisplay0},
      main = Versatile versSpec
    }

definitions :: [BuiltInSpec]
definitions =
  [ gen [] "vadd" [ParamInt],
    gen ["tensor"] "zeros" [ParamIntList],
    gen ["tensor"] "grad" [ParamIntList],
    gen ["tensor"] "zero_grad" [ParamIntList],
    gen ["tensor"] "sub_update" [ParamIntList],
    gen ["tensor"] "count_equal" [ParamIntList],
    gen ["tensor"] "dropout" [ParamIntList],
    versatile "BITupleFirst" "FST" $ VersatileSpec [] 1
      [| do
        (a0v11, _) <- validateTupleValue a0v1
        pure a0v11 |],
    versatile "BITupleSecond" "SND" $ VersatileSpec [] 1
      [| do
        (_, a0v12) <- validateTupleValue a0v1
        pure a0v12 |],
    versatile "BIDeviceGenCudaIfAvailable" "DEVICE.GEN_CUDA_IF_AVAILABLE" $ VersatileSpec [] 1
      [| do
        () <- validateUnitLiteral a0v1
        pure $ A0ValBracket (A1ValLiteral ALitUnit) |], -- TODO: return a value of type `Device`
    versatile "BIMtranspose" "MTRANSPOSE" $ VersatileSpec [ParamInt, ParamInt] 1
      [| do
        mat1 <- validateMat0 a0v1
        case Matrix.transpose p1 p2 mat1 of
          Just mat -> pure $ A0ValLiteral (ALitMat mat)
          Nothing -> bug $ InconsistentAppBuiltInArity1 bi1 a0v1 |]
  ]
