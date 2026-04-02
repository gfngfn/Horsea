module Surface.BindingTime.Core
  ( BindingTimeVar (..),
    BindingTimeConst (..),
    BindingTime (..),
    BITypeVar (..),
    BITypeF (..),
    BITypeMainF (..),
    BITypeBoundVar (..),
    BIPolyTypeF (..),
    BIType,
    BITypeMain,
    BindingTimeEnvEntry (..),
    BindingTimeEnv,
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
    BIPolyTypeVoid,
    BITypeVoid,
    fromStaged0,
    fromStaged1,
    fromStagedPers,
  )
where

import Common.TokenUtil
import Data.List.NonEmpty (NonEmpty)
import Data.List.TwoOrMore (TwoOrMore)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Void (Void, vacuous)
import GHC.Generics
import Staged.Core (Label)
import Staged.Syntax qualified as Staged
import Surface.Syntax
import Prelude

newtype BindingTimeVar = BindingTimeVar Int
  deriving stock (Eq, Ord, Show)

data BindingTimeConst = BT0 | BT1
  deriving stock (Eq, Ord, Show) -- BT0 < BT1

data BindingTime
  = BTConst BindingTimeConst
  | BTVar BindingTimeVar
  deriving stock (Eq, Show, Generic)

newtype BITypeVar = BITypeVar Int
  deriving stock (Eq, Ord, Show)

-- Intermediate, minimal type representations for binding-time analysis
data BITypeF bt tv = BIType bt (BITypeMainF bt tv)
  deriving stock (Functor, Show)

newtype BITypeBoundVar = BITypeBoundVar Int
  deriving stock (Eq, Ord, Show)

data BIPolyTypeF bt = BIPolyType (Set BITypeBoundVar) (BITypeF bt BITypeBoundVar)
  deriving stock (Show)

data BITypeMainF bt tv
  = BITyVar tv
  | BITyBase [BITypeF bt tv]
  | BITyProduct (TwoOrMore (BITypeF bt tv))
  | BITyArrow (BITypeF bt tv) (BITypeF bt tv)
  | BITyOmsArrow (BITypeF bt tv) (BITypeF bt tv)
  | BITyInfArrow (BITypeF bt tv) (BITypeF bt tv)
  deriving stock (Functor, Show)

type BIType = BITypeF BindingTime BITypeVar

type BITypeMain = BITypeMainF BindingTime BITypeVar

data BindingTimeEnvEntry
  = EntryBuiltInPersistent Var (BIPolyTypeF ())
  | EntryBuiltInFixed0 Var BIPolyTypeVoid
  | EntryBuiltInFixed1 Var BITypeVoid
  | EntryLocallyBound BindingTime BIType
  | EntryModule BindingTimeEnv
  deriving stock (Show)

type BindingTimeEnv = Map Var BindingTimeEnvEntry

data BExprF ann bt = BExpr (bt, ann) (BExprMainF ann bt)
  deriving stock (Functor, Show)

data BExprMainF ann bt
  = BLiteral (Literal (BExprF ann bt))
  | BVar ([Var], Var)
  | BConstructor ([Var], Var)
  | BLam (Maybe (Var, BTypeExprF ann bt)) (Maybe Label) (Var, BTypeExprF ann bt) (BExprF ann bt)
  | BApp (BExprF ann bt) (Maybe Label) (BExprF ann bt)
  | BLetIn Var (BExprF ann bt) (BExprF ann bt)
  | BLetTupleIn (TwoOrMore Var) (BExprF ann bt) (BExprF ann bt)
  | BLetOpenIn Var (BExprF ann bt)
  | BSequential (BExprF ann bt) (BExprF ann bt)
  | BTuple (TwoOrMore (BExprF ann bt))
  | BIfThenElse (BExprF ann bt) (BExprF ann bt) (BExprF ann bt)
  | BAs (BExprF ann bt) (BTypeExprF ann bt)
  | BLamInf (Var, BTypeExprF ann bt) (BExprF ann bt)
  | BAppInfGiven (BExprF ann bt) (BExprF ann bt)
  | BAppInfOmitted (BExprF ann bt)
  deriving stock (Functor, Show)

data BTypeExprF ann bt = BTypeExpr (bt, ann) (BTypeExprMainF ann bt)
  deriving stock (Functor, Show)

data BTypeExprMainF ann bt
  = BTyName TypeName [BArgForTypeF ann bt]
  | BTyArrow (Maybe Label) (Maybe Var, BTypeExprF ann bt) (BTypeExprF ann bt)
  | BTyOmsArrow Label (Maybe Var, BTypeExprF ann bt) (BTypeExprF ann bt)
  | BTyInfArrow (Var, BTypeExprF ann bt) (BTypeExprF ann bt)
  | BTyRefinement Var (BTypeExprF ann bt) (BExprF ann bt)
  | BTyProduct (BTypeExprF ann bt) (NonEmpty (ann, BTypeExprF ann bt))
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

-- For built-in values.
type BIPolyTypeVoid = BIPolyTypeF BindingTimeConst

-- For built-in values.
type BITypeVoid = BITypeF BindingTimeConst Void

-- Accepts only top-level universal quantifications.
fromStaged0 :: Staged.Ass0TypeExpr -> Maybe BIPolyTypeVoid
fromStaged0 = goPoly 0 Map.empty
  where
    goPoly :: Int -> Map Staged.AssTypeVar BITypeBoundVar -> Staged.Ass0TypeExpr -> Maybe BIPolyTypeVoid
    goPoly i vars = \case
      Staged.A0TyImplicitForAll atyvar a0tye ->
        goPoly (i + 1) (Map.insert atyvar (BITypeBoundVar i) vars) a0tye
      a0tye ->
        BIPolyType (Set.fromList (Map.elems vars)) <$> go a0tye
        where
          go :: Staged.Ass0TypeExpr -> Maybe (BITypeF BindingTimeConst BITypeBoundVar)
          go = \case
            Staged.A0TyPrim _a0tyPrim _maybePred ->
              pure . wrap0 $ BITyBase []
            Staged.A0TyVar atyvar ->
              case Map.lookup atyvar vars of
                Nothing -> error "bug: fromStaged0, type variable not found"
                Just bitv -> pure . wrap0 $ BITyVar bitv
            Staged.A0TyList a0tye' _maybePred -> do
              bity <- go a0tye'
              pure . wrap0 $ BITyBase [bity]
            Staged.A0TyMaybe a0tye' -> do
              bity <- go a0tye'
              pure . wrap0 $ BITyBase [bity]
            Staged.A0TyProduct a0tyes -> do
              bitys <- mapM go a0tyes
              pure $ wrap0 (BITyProduct bitys)
            Staged.A0TyArrow _labelOpt (_, a0tye1) a0tye2 ->
              wrap0 <$> (BITyArrow <$> go a0tye1 <*> go a0tye2)
            Staged.A0TyInfArrow (_, a0tye1) a0tye2 ->
              wrap0 <$> (BITyInfArrow <$> go a0tye1 <*> go a0tye2)
            Staged.A0TyOmsArrow _label (_, _a0tye1) _a0tye2 ->
              error "TODO: fromStaged0, Staged.A0TyOmsArrow"
            Staged.A0TyCode a1tye ->
              pure $ vacuous $ fromStaged1 a1tye
            Staged.A0TyImplicitForAll _atyvar _a0tye ->
              Nothing

    wrap0 = BIType BT0

fromStaged1 :: Staged.Ass1TypeExpr -> BITypeVoid
fromStaged1 = \case
  Staged.A1TyPrim _a1tyPrim ->
    wrap1 $ BITyBase []
  Staged.A1TyList a1tye' ->
    wrap1 $ BITyBase [fromStaged1 a1tye']
  Staged.A1TyMaybe a1tye' ->
    wrap1 $ BITyBase [fromStaged1 a1tye']
  Staged.A1TyVar _atyvar ->
    -- Handles order-0 type variables only:
    wrap1 $ BITyBase []
  Staged.A1TyProduct a1tyes ->
    wrap1 $ BITyProduct (fmap fromStaged1 a1tyes)
  Staged.A1TyArrow _labelOpt a1tye1 a1tye2 ->
    wrap1 $ BITyArrow (fromStaged1 a1tye1) (fromStaged1 a1tye2)
  Staged.A1TyImplicitForAll _atyvar a1tye2 ->
    -- TODO: support type application
    fromStaged1 a1tye2
  where
    wrap1 = BIType BT1

-- Accepts only top-level universal quantifications.
fromStagedPers :: Staged.AssPersTypeExpr -> Maybe (BIPolyTypeF ())
fromStagedPers = goPoly 0 Map.empty
  where
    goPoly :: Int -> Map Staged.AssTypeVar BITypeBoundVar -> Staged.AssPersTypeExpr -> Maybe (BIPolyTypeF ())
    goPoly i vars = \case
      Staged.APersTyImplicitForAll atyvar aPtye ->
        goPoly (i + 1) (Map.insert atyvar (BITypeBoundVar i) vars) aPtye
      aPtye ->
        BIPolyType (Set.fromList (Map.elems vars)) <$> go aPtye
        where
          go :: Staged.AssPersTypeExpr -> Maybe (BITypeF () BITypeBoundVar)
          go = \case
            Staged.APersTyPrim _aPtyPrim ->
              pure . wrapP $ BITyBase []
            Staged.APersTyVar atyvar ->
              case Map.lookup atyvar vars of
                Nothing -> error "bug: fromStagedPers, type variable not found"
                Just bitv -> pure . wrapP $ BITyVar bitv
            Staged.APersTyList aPtye' -> do
              bity <- go aPtye'
              pure . wrapP $ BITyBase [bity]
            Staged.APersTyMaybe aPtye' -> do
              bity <- go aPtye'
              pure . wrapP $ BITyBase [bity]
            Staged.APersTyProduct aPtyes -> do
              bitys <- mapM go aPtyes
              pure $ wrapP (BITyProduct bitys)
            Staged.APersTyArrow _labelOpt aPtye1 aPtye2 ->
              wrapP <$> (BITyArrow <$> go aPtye1 <*> go aPtye2)
            Staged.APersTyImplicitForAll _atyvar _aPtye2 ->
              Nothing

    wrapP = BIType ()
