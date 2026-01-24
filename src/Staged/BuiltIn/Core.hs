{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.Core
  ( BuiltIn (..),
    BuiltInArity1 (..),
    BuiltInArity2 (..),
    BuiltInArity3 (..),
    BuiltInArity5 (..),
    BuiltInArity7 (..),
    BuiltInArity8 (..),
    Ass0PartialBuiltInApp (..),
    Ass0PartialBuiltInAppArity1 (..),
    Ass0PartialBuiltInAppArity2 (..),
    Ass0PartialBuiltInAppArity3 (..),
    Ass0PartialBuiltInAppArity4 (..),
    Ass0PartialBuiltInAppArity5 (..),
    Ass0PartialBuiltInAppArity6 (..),
    Ass0PartialBuiltInAppArity7 (..),
    Ass0PartialBuiltInAppArity8 (..),
    Foo (..), -- TODO: remove this
    Ass1BuiltIn (..),
    validateExternalName0,
    validateExternalName1,
    unliftBuiltInName,
  )
where

import Data.Text (Text)
import Staged.BuiltIn.CompileTime (deriveDecs)
import Staged.BuiltIn.Definitions (definitions)
import Prelude

$(deriveDecs definitions)

data BuiltIn
  = BuiltInArity1 BuiltInArity1
  | BuiltInArity2 BuiltInArity2
  | BuiltInArity3 BuiltInArity3
  | BuiltInArity5 BuiltInArity5
  | BuiltInArity7 BuiltInArity7
  | BuiltInArity8 BuiltInArity8
  | BuiltInOther Text -- TODO: remove this
  deriving stock (Eq, Show)

data BuiltInArity5
  = BIDatasetHelperGenTrainBatch
  deriving stock (Eq, Show)

data BuiltInArity7
  = BIDatasetHelperGenBatchAccuracy
  | BITensorGenMaxPool2d
  deriving stock (Eq, Show)

data BuiltInArity8
  = BILayerGenConv2d
  deriving stock (Eq, Show)

data Ass0PartialBuiltInApp val
  = A0PartialBuiltInAppArity1 (Ass0PartialBuiltInAppArity1 val)
  | A0PartialBuiltInAppArity2 (Ass0PartialBuiltInAppArity2 val)
  | A0PartialBuiltInAppArity3 (Ass0PartialBuiltInAppArity3 val)
  | A0PartialBuiltInAppArity4 (Ass0PartialBuiltInAppArity4 val)
  | A0PartialBuiltInAppArity5 (Ass0PartialBuiltInAppArity5 val)
  | A0PartialBuiltInAppArity6 (Ass0PartialBuiltInAppArity6 val)
  | A0PartialBuiltInAppArity7 (Ass0PartialBuiltInAppArity7 val)
  | A0PartialBuiltInAppArity8 (Ass0PartialBuiltInAppArity8 val)
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity1 val
  = PartialBuiltInAppArity1Nil BuiltInArity1
  | PartialBuiltInAppArity1Cons (Ass0PartialBuiltInAppArity2 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity2 val
  = PartialBuiltInAppArity2Nil BuiltInArity2
  | PartialBuiltInAppArity2Cons (Ass0PartialBuiltInAppArity3 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity3 val
  = PartialBuiltInAppArity3Nil BuiltInArity3
  | PartialBuiltInAppArity3Cons (Ass0PartialBuiltInAppArity4 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity4 val
  = PartialBuiltInAppArity4Cons (Ass0PartialBuiltInAppArity5 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity5 val
  = PartialBuiltInAppArity5Nil BuiltInArity5
  | PartialBuiltInAppArity5Cons (Ass0PartialBuiltInAppArity6 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity6 val
  = PartialBuiltInAppArity6Cons (Ass0PartialBuiltInAppArity7 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity7 val
  = PartialBuiltInAppArity7Nil BuiltInArity7
  | PartialBuiltInAppArity7Cons (Ass0PartialBuiltInAppArity8 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity8 val
  = PartialBuiltInAppArity8Nil BuiltInArity8
  deriving stock (Eq, Show, Functor)

data Ass1BuiltIn
  = A1BIVadd Int
  | A1BIVconcat Int Int
  | A1BIMtranspose Int Int
  | A1BIMconcatVert Int Int Int
  | A1BITensorZeros [Int]
  | A1BITensorAdd [Int] [Int]
  | A1BITensorMult [Int] [Int]
  | A1BITensorMm Int Int Int
  | A1BITensorGrad [Int]
  | A1BITensorZeroGrad [Int]
  | A1BITensorSubUpdate [Int]
  | A1BITensorArgmax [Int] Int
  | A1BITensorCrossEntropyForLogits Int Int
  | A1BITensorCountEqual [Int]
  | A1BITensorDropout [Int]
  | A1BITensorReshape [Int] [Int]
  | A1BIAdd
  | A1BISub
  | A1BIMult
  | A1BIDiv
  | A1BIFloatDiv
  | A1BIMod
  | A1BILeq
  | A1BIEqual
  | A1BIFloat
  | A1BIPrintFloat
  | A1BIPrintString
  | A1BIRange
  | A1BIListAppend
  | A1BIListIter
  | A1BITensorF
  | A1BITensorBackward
  | A1BITensorNoGrad
  | A1BITensorFloatValue
  | A1BITensorMaxPool2d Int Int Int Int Int Int Int Int Int Int
  | A1BILayerActivationRelu
  | A1BILayerActivationNone
  | A1BILayerLinear [Int] Int Int
  | A1BILayerForward [Int] [Int]
  | A1BILayerConv2d Int Int Int Int Int Int Int Int
  | A1BIVarStoreCreate
  | A1BIOptimizerAdam
  | A1BIOptimizerBackwardStep
  | A1BIDatasetHelperTrainBatch Int Int [Int] [Int] Int
  | A1BIDatasetHelperBatchAccuracy Int Int [Int] [Int] Int Int
  | A1BIMnistHelperTrainImages
  | A1BIMnistHelperTrainLabels
  | A1BIMnistHelperTestImages
  | A1BIMnistHelperTestLabels
  | A1BuiltInOther Text -- TODO: remove this
  deriving stock (Eq, Show)

unliftBuiltInName :: Ass1BuiltIn -> BuiltIn
unliftBuiltInName = \case
  A1BIVadd n -> arity2 (BIVadd n)
  A1BIVconcat m n -> arity2 (BIVconcat m n)
  A1BIMtranspose m n -> arity1 (BIMtranspose m n)
  A1BIMconcatVert m1 m2 n -> arity2 (BIMconcatVert m1 m2 n)
  A1BITensorAdd ns1 ns2 ->
    if ns1 == ns2
      then arity2 (BITensorAdd ns1)
      else error $ "TODO: unliftBuiltInName, A1BITensorAdd, broadcast, " ++ show ns1 ++ " and " ++ show ns2
  A1BITensorMm k m n -> arity2 (BITensorMm k m n)
  A1BIAdd -> arity2 BIAdd
  A1BISub -> arity2 BISub
  A1BIMult -> arity2 BIMult
  A1BIDiv -> arity2 BIDiv
  A1BIMod -> arity2 BIMod
  A1BILeq -> arity2 BILeq
  A1BIEqual -> arity2 BIEqual
  A1BIListAppend -> arity2 BIListAppend
  A1BIListIter -> arity2 BIListIter
  a1builtInName -> error $ "TODO: unliftBuiltInName, " ++ show a1builtInName
  where
    arity1 = BuiltInArity1
    arity2 = BuiltInArity2

validateExternalName0 :: Text -> Maybe BuiltIn
validateExternalName0 = \case
  "int_add" -> arity2 BIAdd
  "int_sub" -> arity2 BISub
  "int_mult" -> arity2 BIMult
  "int_div" -> arity2 BIDiv
  "int_mod" -> arity2 BIMod
  "int_leq" -> arity2 BILeq
  "int_equal" -> arity2 BIEqual
  "gen_vadd" -> arity1 BIGenVadd
  "gen_vconcat" -> arity2 BIGenVconcat
  "gen_mtranspose" -> arity2 BIGenMtranspose
  "gen_mconcat_vert" -> arity3 BIGenMconcatVert
  "drop_at" -> arity2 BIDropAt
  "broadcastable" -> arity2 BIBroadcastable
  "broadcast" -> arity2 BIBroadcast
  "reshapeable" -> arity2 BIReshapeable
  "list__cons" -> arity2 BIListCons
  "list__append" -> arity2 BIListAppend
  "list__iter" -> arity2 BIListIter
  "tuple__first" -> arity1 BIFst
  "tuple__second" -> arity1 BISnd
  "device__gen_cuda_if_available" -> arity1 BIDeviceGenCudaIfAvailable
  "layer__gen_forward" -> arity2 BILayerGenForward
  "layer__gen_conv2d_" -> arity8 BILayerGenConv2d
  "layer__gen_linear" -> arity3 BILayerGenLinear
  "tensor__gen_zeros" -> arity1 BITensorGenZeros
  "tensor__gen_add" -> arity2 BITensorGenAdd
  "tensor__gen_mult" -> arity2 BITensorGenMult
  "tensor__gen_mm" -> arity3 BITensorGenMm
  "tensor__gen_grad" -> arity1 BITensorGenGrad
  "tensor__gen_zero_grad" -> arity1 BITensorGenZeroGrad
  "tensor__gen_sub_update" -> arity1 BITensorGenSubUpdate
  "tensor__gen_argmax" -> arity2 BITensorGenArgmax
  "tensor__gen_cross_entropy_for_logits" -> arity2 BITensorGenCrossEntropyForLogits
  "tensor__gen_count_equal" -> arity1 BITensorGenCountEqual
  "tensor__gen_dropout" -> arity1 BITensorGenDropout
  "tensor__gen_reshape" -> arity2 BITensorGenReshape
  "tensor__gen_max_pool2d" -> arity7 BITensorGenMaxPool2d
  "dataset_helper__gen_train_batch" -> arity5 BIDatasetHelperGenTrainBatch
  "dataset_helper__gen_batch_accuracy" -> arity7 BIDatasetHelperGenBatchAccuracy
  s -> pure $ BuiltInOther s
  --  _ -> Nothing
  where
    arity1 = pure . BuiltInArity1
    arity2 = pure . BuiltInArity2
    arity3 = pure . BuiltInArity3
    arity5 = pure . BuiltInArity5
    arity7 = pure . BuiltInArity7
    arity8 = pure . BuiltInArity8

validateExternalName1 :: Text -> Maybe Ass1BuiltIn
validateExternalName1 = \case
  "int_add" -> pure A1BIAdd
  "int_sub" -> pure A1BISub
  "int_mult" -> pure A1BIMult
  "int_div" -> pure A1BIDiv
  "float_div" -> pure A1BIFloatDiv
  "int_mod" -> pure A1BIMod
  "int_leq" -> pure A1BILeq
  "int_equal" -> pure A1BIEqual
  "float" -> pure A1BIFloat
  "print_float" -> pure A1BIPrintFloat
  "print_string" -> pure A1BIPrintString
  "range" -> pure A1BIRange
  "list__append" -> pure A1BIListAppend
  "list__iter" -> pure A1BIListIter
  "tensor__f" -> pure A1BITensorF
  "tensor__backward" -> pure A1BITensorBackward
  "tensor__no_grad" -> pure A1BITensorNoGrad
  "tensor__float_value" -> pure A1BITensorFloatValue
  "layer__activation__relu" -> pure A1BILayerActivationRelu
  "layer__activation__none" -> pure A1BILayerActivationNone
  "var_store__create" -> pure A1BIVarStoreCreate
  "optimizer__adam" -> pure A1BIOptimizerAdam
  "optimizer__backward_step" -> pure A1BIOptimizerBackwardStep
  "mnist_helper__train_images" -> pure A1BIMnistHelperTrainImages
  "mnist_helper__train_labels" -> pure A1BIMnistHelperTrainLabels
  "mnist_helper__test_images" -> pure A1BIMnistHelperTestImages
  "mnist_helper__test_labels" -> pure A1BIMnistHelperTestLabels
  s -> pure $ A1BuiltInOther s

-- _ -> Nothing
