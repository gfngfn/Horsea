{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.Definitions
  ( definitions,
  )
where

import Staged.BuiltIn.CompileTime
import Util.Matrix qualified as Matrix
import Prelude

gen :: String -> String -> GenSpec -> BuiltInSpec
gen constructor0 constructorDisplay0 genSpec =
  BuiltInSpec
    { common = Common {constructor0, constructorDisplay0},
      main = Gen genSpec
    }

versatile :: String -> String -> VersatileSpec -> BuiltInSpec
versatile constructor0 constructorDisplay0 versSpec =
  BuiltInSpec
    { common = Common {constructor0, constructorDisplay0},
      main = Versatile versSpec
    }

definitions :: [BuiltInSpec]
definitions =
  [ gen "BIGenVadd" "GEN_VADD" $ GenSpec [ParamInt] "A1BIVadd" "vadd",
    gen "BITensorGenZeros" "TENSOR.GEN_ZEROS" $ GenSpec [ParamIntList] "A1BITensorZeros" "Tensor.zeros",
    gen "BITensorGenGrad" "TENSOR.GEN_GRAD" $ GenSpec [ParamIntList] "A1BITensorGrad" "Tensor.grad",
    gen "BITensorGenZeroGrad" "TENSOR.GEN_ZERO_GRAD" $ GenSpec [ParamIntList] "A1BITensorZeroGrad" "Tensor.zero_grad",
    gen "BITensorGenSubUpdate" "TENSOR.GEN_SUB_UPDATE" $ GenSpec [ParamIntList] "A1BITensorSubUpdate" "Tensor.sub_update",
    gen "BITensorGenCountEqual" "TENSOR.GEN_COUNT_EQUAL" $ GenSpec [ParamIntList] "A1BITensorCountEqual" "Tensor.count_equal",
    gen "BITensorGenDropout" "TENSOR.GEN_DROPOUT" $ GenSpec [ParamIntList] "A1BITensorDropout" "Tensor.dropout",
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
