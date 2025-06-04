module Surface.BindingTime.Core
  ( BindingTimeVar (..),
    BindingTimeConst (..),
    BindingTime (..),
    BITypeF (..),
    BITypeMainF (..),
    BIType,
    BindingTimeEnvEntry (..),
    BindingTimeEnv,
    BExpr,
    BTypeExpr,
    BArgForType,
    BITypeVoid,
    fromStaged0,
    fromStaged1,
    fromStagedPers,
  )
where

import Data.Map (Map)
import GHC.Generics
import Staged.Syntax qualified as Staged
import Surface.Syntax
import Util.TokenUtil
import Prelude

newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Eq, Ord, Show)

data BindingTimeConst = BT0 | BT1
  deriving stock (Eq, Ord, Show) -- BT0 < BT1

data BindingTime
  = BTConst BindingTimeConst
  | BTVar BindingTimeVar
  deriving stock (Eq, Show, Generic)

-- Intermediate, minimal type representations for binding-time analysis
data BITypeF bt = BIType bt (BITypeMainF bt)
  deriving stock (Show)

data BITypeMainF bt
  = BITyBase [BITypeF bt]
  | BITyProduct (BITypeF bt) (BITypeF bt) -- TODO: generalize product types
  | BITyArrow (BITypeF bt) (BITypeF bt)
  | BITyOptArrow (BITypeF bt) (BITypeF bt)
  deriving stock (Show)

type BIType = BITypeF BindingTime

data BindingTimeEnvEntry
  = EntryBuiltInPersistent Var (BITypeF ())
  | EntryBuiltInFixed Var BindingTimeConst (BITypeF BindingTimeConst)
  | EntryLocallyBound BindingTime BIType
  | EntryModule BindingTimeEnv
  deriving stock (Show)

type BindingTimeEnv = Map Var BindingTimeEnvEntry

type BExpr = ExprF (BindingTime, Span)

type BTypeExpr = TypeExprF (BindingTime, Span)

type BArgForType = ArgForTypeF (BindingTime, Span)

-- For built-in values.
type BITypeVoid = BITypeF BindingTimeConst

fromStaged0 :: Staged.Ass0TypeExpr -> BITypeVoid
fromStaged0 = \case
  Staged.A0TyPrim _a0tyPrim _maybePred ->
    wrap0 $ BITyBase []
  Staged.A0TyVar _atyvar ->
    -- Handles order-0 type variables only:
    wrap0 $ BITyBase []
  Staged.A0TyList a0tye' _maybePred ->
    wrap0 $ BITyBase [fromStaged0 a0tye']
  Staged.A0TyProduct a0tye1 a0tye2 ->
    wrap0 $ BITyProduct (fromStaged0 a0tye1) (fromStaged0 a0tye2)
  Staged.A0TyArrow (_, a0tye1) a0tye2 ->
    wrap0 $ BITyArrow (fromStaged0 a0tye1) (fromStaged0 a0tye2)
  Staged.A0TyOptArrow (_, a0tye1) a0tye2 ->
    wrap0 $ BITyOptArrow (fromStaged0 a0tye1) (fromStaged0 a0tye2)
  Staged.A0TyCode a1tye ->
    fromStaged1 a1tye
  Staged.A0TyImplicitForAll _atyvar a0tye ->
    -- TODO: support type application
    fromStaged0 a0tye
  where
    wrap0 = BIType BT0

fromStaged1 :: Staged.Ass1TypeExpr -> BITypeVoid
fromStaged1 = \case
  Staged.A1TyPrim _a1tyPrim ->
    wrap1 $ BITyBase []
  Staged.A1TyList a1tye' ->
    wrap1 $ BITyBase [fromStaged1 a1tye']
  Staged.A1TyVar _atyvar ->
    -- Handles order-0 type variables only:
    wrap1 $ BITyBase []
  Staged.A1TyProduct a1tye1 a1tye2 ->
    wrap1 $ BITyProduct (fromStaged1 a1tye1) (fromStaged1 a1tye2)
  Staged.A1TyArrow a1tye1 a1tye2 ->
    wrap1 $ BITyArrow (fromStaged1 a1tye1) (fromStaged1 a1tye2)
  Staged.A1TyImplicitForAll _atyvar a1tye2 ->
    -- TODO: support type application
    fromStaged1 a1tye2
  where
    wrap1 = BIType BT1

fromStagedPers :: Staged.AssPersTypeExpr -> BITypeF ()
fromStagedPers = \case
  Staged.APersTyPrim _a0tyPrim ->
    wrapP $ BITyBase []
  Staged.APersTyVar _atyvar ->
    -- Handles order-0 type variables only:
    wrapP $ BITyBase []
  Staged.APersTyList aPtye' ->
    wrapP $ BITyBase [fromStagedPers aPtye']
  Staged.APersTyProduct aPtye1 aPtye2 ->
    wrapP $ BITyProduct (fromStagedPers aPtye1) (fromStagedPers aPtye2)
  Staged.APersTyArrow aPtye1 aPtye2 ->
    wrapP $ BITyArrow (fromStagedPers aPtye1) (fromStagedPers aPtye2)
  Staged.APersTyImplicitForAll _atyvar aPtye2 ->
    -- TODO: support type application
    fromStagedPers aPtye2
  where
    wrapP = BIType ()
