{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.Definitions
  ( definitions,
  )
where

import Staged.BuiltIn.CompileTime
import Util.Matrix qualified as Matrix
import Prelude

definitions :: [BuiltInSpec]
definitions =
  [ gen "BIGenVadd" $ GenSpec [ParamInt] "A1BIVadd",
    gen "BITensorGenZeros" $ GenSpec [ParamIntList] "A1BITensorZeros",
    gen "BITensorGenGrad" $ GenSpec [ParamIntList] "A1BITensorGrad",
    gen "BITensorGenZeroGrad" $ GenSpec [ParamIntList] "A1BITensorZeroGrad",
    gen "BITensorGenSubUpdate" $ GenSpec [ParamIntList] "A1BITensorSubUpdate",
    gen "BITensorGenCountEqual" $ GenSpec [ParamIntList] "A1BITensorCountEqual",
    gen "BITensorGenDropout" $ GenSpec [ParamIntList] "A1BITensorDropout",
    versatile "BITupleFirst" $ VersatileSpec [] 1 $
      [| do
        (a0v11, _) <- validateTupleValue a0v1
        pure a0v11 |],
    versatile "BITupleSecond" $ VersatileSpec [] 1 $
      [| do
        (_, a0v12) <- validateTupleValue a0v1
        pure a0v12 |],
    versatile "BIDeviceGenCudaIfAvailable" $ VersatileSpec [] 1 $
      [| do
        () <- validateUnitLiteral a0v1
        pure $ A0ValBracket (A1ValLiteral ALitUnit) |], -- TODO: return a value of type `Device`
    versatile "BIMtranspose" $ VersatileSpec [ParamInt, ParamInt] 1 $
      [| do
        mat1 <- validateMat0 a0v1
        case Matrix.transpose p1 p2 mat1 of
          Just mat -> pure $ A0ValLiteral (ALitMat mat)
          Nothing -> bug $ InconsistentAppBuiltInArity1 bi1 a0v1 |]
  ]
