module Staged.BuiltIn.Definitions
  ( definitions,
  )
where

import Staged.BuiltIn.CompileTime
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
    versatile "BITupleFirst" $ VersatileSpec [] 1,
    versatile "BITupleSecond" $ VersatileSpec [] 1,
    versatile "BIDeviceGenCudaIfAvailable" $ VersatileSpec [] 1,
    versatile "BIMtranspose" $ VersatileSpec [ParamInt, ParamInt] 1
  ]
