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
  | A1BIIntAdd
  | A1BIIntSub
  | A1BIIntMult
  | A1BIIntDiv
  | A1BIFloatDiv
  | A1BIIntMod
  | A1BIIntLeq
  | A1BIIntEqual
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
  | A1BITensorMaxPool2d Int Int Int Int (Int, Int) (Int, Int) (Int, Int)
  | A1BILayerActivationRelu
  | A1BILayerActivationNone
  | A1BILayerLinear [Int] Int Int
  | A1BILayerForward [Int] [Int]
  | A1BILayerConv2d_ Int Int Int Int Int Int Int Int
  | A1BIVarStoreCreate
  | A1BIOptimizerAdam
  | A1BIOptimizerBackwardStep
  | A1BIDatasetHelperTrainBatch Int Int [Int] [Int] Int
  | A1BIDatasetHelperBatchAccuracy Int Int [Int] [Int] Int Int ()
  | A1BIMnistHelperTrainImages
  | A1BIMnistHelperTrainLabels
  | A1BIMnistHelperTestImages
  | A1BIMnistHelperTestLabels

  | A1BIFst
  | A1BISnd
  | A1BIAnd
  | A1BIListMap
  | A1BIListCons

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
  A1BIIntAdd -> arity2 BIIntAdd
  A1BIIntSub -> arity2 BIIntSub
  A1BIIntMult -> arity2 BIIntMult
  A1BIIntDiv -> arity2 BIIntDiv
  A1BIIntMod -> arity2 BIIntMod
  A1BIIntLeq -> arity2 BIIntLeq
  A1BIIntEqual -> arity2 BIIntEqual
  A1BIListAppend -> arity2 BIListAppend
  A1BIListIter -> arity2 BIListIter
  a1builtInName -> error $ "TODO: unliftBuiltInName, " ++ show a1builtInName
  where
    arity1 = BuiltInArity1
    arity2 = BuiltInArity2

{-
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
-}
