{-# LANGUAGE TemplateHaskell #-}

module Staged.BuiltIn.Core
  ( BuiltIn (..),
    BuiltInArity1 (..),
    BuiltInArity2 (..),
    BuiltInArity3 (..),
    BuiltInArity5 (..),
    BuiltInArity6 (..),
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
  | BuiltInArity6 BuiltInArity6
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
  = PartialBuiltInAppArity6Nil BuiltInArity6
  | PartialBuiltInAppArity6Cons (Ass0PartialBuiltInAppArity7 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity7 val
  = PartialBuiltInAppArity7Nil BuiltInArity7
  | PartialBuiltInAppArity7Cons (Ass0PartialBuiltInAppArity8 val) val
  deriving stock (Eq, Show, Functor)

data Ass0PartialBuiltInAppArity8 val
  = PartialBuiltInAppArity8Nil BuiltInArity8
  deriving stock (Eq, Show, Functor)

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
