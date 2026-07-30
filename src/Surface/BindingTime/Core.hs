module Surface.BindingTime.Core
  ( BindingTimeVar (..),
    BindingTimeConst (..),
    BindingTime (..),
    BITypeVar (..),
    BITypeF (..),
    BITypeMainF (..),
    BITypeBoundVar (..),
    BIPolyTypeF (..),
    BITypeParam (..),
    BIParameterizedTypeF (..),
    BIType,
    BITypeMain,
    BIPolyType,
    BIMonoType,
    BExprF (..),
    BExprMainF (..),
    BTypeExprF (..),
    BTypeExprMainF (..),
    BArgForTypeF (..),
    BExpr,
    BExprMain,
    BTypeExpr,
    BTypeExprMain,
    BArgForType,
  )
where

import Common.TokenUtil
import Data.List.NonEmpty (NonEmpty)
import Data.List.TwoOrMore (TwoOrMore)
import Data.Map (Map)
import Data.Set (Set)
import Data.Void (Void)
import GHC.Generics
import Staged.Core (Label)
import Surface.Syntax (Literal, ModuleName, TypeName, Var)
import Prelude

-- | Binding-time variables that can be resolved to 0 or 1.
newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Eq, Ord, Show)

data BindingTimeConst = BT0 | BT1
  deriving stock (Eq, Ord, Show) -- BT0 < BT1

data BindingTime
  = BTConst BindingTimeConst
  | BTVar BindingTimeVar
  deriving stock (Eq, Show, Generic)

-- | BTA types, i.e., intermediate, minimal type representations for binding-time analysis.
data BITypeF bt tv = BIType bt (BITypeMainF bt tv)
  deriving stock (Functor, Show)

data BITypeMainF bt tv
  = BITyVar tv
  | BITyBase [BITypeF bt tv]
  | BITyProduct (TwoOrMore (BITypeF bt tv))
  | BITyRecord (Map Label (BITypeF bt tv))
  | BITyArrow (BITypeF bt tv) (BITypeF bt tv)
  | BITyOmsArrow Label (BITypeF bt tv) (BITypeF bt tv)
  | BITyInfArrow (BITypeF bt tv) (BITypeF bt tv)
  deriving stock (Functor, Show)

newtype BITypeVar = BITypeVar Int
  deriving stock (Eq, Ord, Show)

-- | Universally quantified BTA type variables.
newtype BITypeBoundVar = BITypeBoundVar Int
  deriving stock (Eq, Ord, Show)

-- | Prenex-polymorphic BTA types.
data BIPolyTypeF bt = BIPolyType (Set BITypeBoundVar) (BITypeF bt BITypeBoundVar)
  deriving stock (Show)

data BITypeParam
  = BITypeParamType BITypeBoundVar
  | BITypeParamVal0 BIMonoType
  deriving stock (Show)

data BIParameterizedTypeF bt = BIParameterizedType [BITypeParam] (BITypeF bt BITypeBoundVar)
  deriving stock (Show)

type BIType = BITypeF BindingTime BITypeVar

type BITypeMain = BITypeMainF BindingTime BITypeVar

-- | Prenex-polymorphic BTA types for built-in values.
type BIPolyType = BIPolyTypeF BindingTimeConst

-- | Monomorphic BTA types for type parameters.
type BIMonoType = BITypeF BindingTimeConst Void

-- | Intermediate term expressions produced by binding-time analysis.
data BExprF ann bt = BExpr (bt, ann) (BExprMainF ann bt)
  deriving stock (Functor, Show)

data BExprMainF ann bt
  = BLiteral (Literal (BExprF ann bt))
  | BVar ([ModuleName], Var)
  | BConstructor ([ModuleName], Var)
  | BLam (Maybe (Var, BTypeExprF ann bt)) (Maybe Label) (Var, BTypeExprF ann bt) (BExprF ann bt)
  | BApp (BExprF ann bt) (Maybe Label) (BExprF ann bt)
  | BLetIn Var (BExprF ann bt) (BExprF ann bt)
  | BLetTupleIn (TwoOrMore Var) (BExprF ann bt) (BExprF ann bt)
  | BLetOpenIn Var (BExprF ann bt)
  | BSequential (BExprF ann bt) (BExprF ann bt)
  | BTuple (TwoOrMore (BExprF ann bt))
  | BRecord (Map Label (BExprF ann bt))
  | BFieldProj (BExprF ann bt) Label
  | BIfThenElse (BExprF ann bt) (BExprF ann bt) (BExprF ann bt)
  | BAs (BExprF ann bt) (BTypeExprF ann bt)
  | BLamOms Label (Var, BTypeExprF ann bt) (BExprF ann bt)
  | BAppOms (BExprF ann bt) Label (BExprF ann bt)
  | BLamInf (Var, BTypeExprF ann bt) (BExprF ann bt)
  | BAppInfGiven (BExprF ann bt) (BExprF ann bt)
  | BAppInfOmitted (BExprF ann bt)
  deriving stock (Functor, Show)

-- | Intermediate type expressions produced by binding-time analysis.
data BTypeExprF ann bt = BTypeExpr (bt, ann) (BTypeExprMainF ann bt)
  deriving stock (Functor, Show)

data BTypeExprMainF ann bt
  = BTyName (ann, ([ModuleName], TypeName)) [BArgForTypeF ann bt]
  | BTyArrow (Maybe Label) (Maybe Var, BTypeExprF ann bt) (BTypeExprF ann bt)
  | BTyOmsArrow Label (Maybe Var, BTypeExprF ann bt) (BTypeExprF ann bt)
  | BTyInfArrow (Var, BTypeExprF ann bt) (BTypeExprF ann bt)
  | BTyRefinement Var (BTypeExprF ann bt) (BExprF ann bt)
  | BTyProduct (BTypeExprF ann bt) (NonEmpty (ann, BTypeExprF ann bt))
  | BTyRecord (Map Label (BTypeExprF ann bt))
  deriving stock (Functor, Show)

data BArgForTypeF ann bt
  = BExprArg (BExprF ann bt)
  | BTypeExprArg (BTypeExprF ann bt)
  deriving stock (Functor, Show)

type BExpr = BExprF Span BindingTime

type BExprMain = BExprMainF Span BindingTime

type BTypeExpr = BTypeExprF Span BindingTime

type BTypeExprMain = BTypeExprMainF Span BindingTime

type BArgForType = BArgForTypeF Span BindingTime
