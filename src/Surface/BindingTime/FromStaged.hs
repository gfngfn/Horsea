module Surface.BindingTime.FromStaged
  ( fromStaged0,
    fromStaged0Body,
    fromStaged1,
    fromStaged1Body,
    fromStagedPers,
    makeBindingTimeEnvFromStub,
  )
where

import Control.Monad (foldM)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Staged.Syntax qualified as Staged
import Staged.Typechecker.SigRecord (Ass0Metadata (..), Ass1Metadata (..), Ass1TypeParam (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, TypeEntry (..), ValEntry (..))
import Staged.Typechecker.SigRecord qualified as SigRecord
import Surface.BindingTime.Core
import Surface.BindingTime.Env (BindingTimeEnv, BindingTimeModuleEntry (..), BindingTimeTypeEntry (..), BindingTimeValueEntry (..))
import Surface.BindingTime.Env qualified as Env
import Prelude

-- Accepts only prenex universal quantifications.
fromStaged0 :: Staged.Ass0TypeExpr -> Maybe BIPolyTypeVoid
fromStaged0 = goPoly 0 Map.empty
  where
    goPoly :: Int -> Map Staged.AssTypeVar BITypeBoundVar -> Staged.Ass0TypeExpr -> Maybe BIPolyTypeVoid
    goPoly i vars0 = \case
      Staged.A0TyForAll atyvar a0tye ->
        goPoly (i + 1) (Map.insert atyvar (BITypeBoundVar i) vars0) a0tye
      a0tye ->
        BIPolyType (Set.fromList (Map.elems vars0)) <$> fromStaged0Body vars0 a0tye

fromStaged0Body :: Map Staged.AssTypeVar BITypeBoundVar -> Staged.Ass0TypeExpr -> Maybe (BITypeF BindingTimeConst BITypeBoundVar)
fromStaged0Body vars0 = go
  where
    go = \case
      Staged.A0TyPrim _a0tyPrim _maybePred ->
        pure . wrap0 $ BITyBase []
      Staged.A0TyVar atyvar ->
        case Map.lookup atyvar vars0 of
          Nothing -> error "bug: fromStaged0Body, type variable not found"
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
      Staged.A0TyRecord a0rty -> do
        rbity <- mapM go a0rty
        pure $ wrap0 (BITyRecord rbity)
      Staged.A0TyArrow _labelOpt (_, a0tye1) a0tye2 ->
        wrap0 <$> (BITyArrow <$> go a0tye1 <*> go a0tye2)
      Staged.A0TyOmsArrow label (_, a0tye1) a0tye2 ->
        wrap0 <$> (BITyOmsArrow label <$> go a0tye1 <*> go a0tye2)
      Staged.A0TyInfArrow (_, a0tye1) a0tye2 ->
        wrap0 <$> (BITyInfArrow <$> go a0tye1 <*> go a0tye2)
      Staged.A0TyCode a1tye ->
        fromStaged1Body Map.empty a1tye
      Staged.A0TyForAll _atyvar _a0tye2 ->
        Nothing

    wrap0 = BIType BT0

-- Accepts only prenex universal quantifications.
fromStaged1 :: Staged.Ass1TypeExpr -> Maybe BIPolyTypeVoid
fromStaged1 = goPoly 0 Map.empty
  where
    goPoly :: Int -> Map Staged.AssTypeVar BITypeBoundVar -> Staged.Ass1TypeExpr -> Maybe BIPolyTypeVoid
    goPoly i vars1 = \case
      Staged.A1TyForAll atyvar a1tye ->
        goPoly (i + 1) (Map.insert atyvar (BITypeBoundVar i) vars1) a1tye
      a1tye ->
        BIPolyType (Set.fromList (Map.elems vars1)) <$> fromStaged1Body vars1 a1tye

fromStaged1Body :: Map Staged.AssTypeVar BITypeBoundVar -> Staged.Ass1TypeExpr -> Maybe (BITypeF BindingTimeConst BITypeBoundVar)
fromStaged1Body vars1 = go
  where
    go :: Staged.Ass1TypeExpr -> Maybe (BITypeF BindingTimeConst BITypeBoundVar)
    go = \case
      Staged.A1TyPrim _a1tyPrim ->
        pure . wrap1 $ BITyBase []
      Staged.A1TyVar atyvar ->
        case Map.lookup atyvar vars1 of
          Nothing -> error "bug: fromStaged1Body, type variable not found"
          Just bitv -> pure . wrap1 $ BITyVar bitv
      Staged.A1TyList a1tye' -> do
        bity1 <- go a1tye'
        pure . wrap1 $ BITyBase [bity1]
      Staged.A1TyMaybe a1tye' -> do
        bity1 <- go a1tye'
        pure . wrap1 $ BITyBase [bity1]
      Staged.A1TyProduct a1tyes -> do
        bitys <- mapM go a1tyes
        pure . wrap1 $ BITyProduct bitys
      Staged.A1TyRecord a1rty -> do
        rbity <- mapM go a1rty
        pure . wrap1 $ BITyRecord rbity
      Staged.A1TyArrow _labelOpt a1tye1 a1tye2 -> do
        bity1 <- go a1tye1
        bity2 <- go a1tye2
        pure . wrap1 $ BITyArrow bity1 bity2
      Staged.A1TyOmsArrow label a1tye1 a1tye2 -> do
        bity1 <- go a1tye1
        bity2 <- go a1tye2
        pure . wrap1 $ BITyOmsArrow label bity1 bity2
      Staged.A1TyForAll _atyvar _a1tye2 ->
        Nothing

    wrap1 = BIType BT1

-- Accepts only top-level universal quantifications.
fromStagedPers :: Staged.AssPersTypeExpr -> Maybe (BIPolyTypeF ())
fromStagedPers = goPoly 0 Map.empty
  where
    goPoly :: Int -> Map Staged.AssTypeVar BITypeBoundVar -> Staged.AssPersTypeExpr -> Maybe (BIPolyTypeF ())
    goPoly i vars = \case
      Staged.APersTyForAll atyvar aPtye ->
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
            Staged.APersTyRecord aPrty -> do
              rbity <- mapM go aPrty
              pure $ wrapP (BITyRecord rbity)
            Staged.APersTyArrow _labelOpt aPtye1 aPtye2 ->
              wrapP <$> (BITyArrow <$> go aPtye1 <*> go aPtye2)
            Staged.APersTyForAll _atyvar _aPtye2 ->
              Nothing

    wrapP = BIType ()

makeBindingTimeEnvFromStub :: SigRecord -> BindingTimeEnv
makeBindingTimeEnvFromStub =
  SigRecord.fold
    ( \varVal entry bindingTimeEnv ->
        case entry of
          Ass0Entry a0tye a0metadataOpt ->
            let x =
                  -- Uses the same name if not specified:
                  case a0metadataOpt of
                    Left Ass0Metadata {ass0surfaceName} -> fromMaybe varVal ass0surfaceName
                    Right _ -> varVal
             in case fromStaged0 a0tye of
                  Nothing ->
                    -- TODO (error): emit a warning
                    bindingTimeEnv
                  Just biptyVoid ->
                    Env.addVal
                      x
                      (BTValBuiltInFixed0 varVal biptyVoid)
                      bindingTimeEnv
          Ass1Entry a1tye a1metadataOpt ->
            let x =
                  -- Uses the same name if not specified:
                  case a1metadataOpt of
                    Left Ass1Metadata {ass1surfaceName} -> fromMaybe varVal ass1surfaceName
                    Right _ -> varVal
             in case fromStaged1 a1tye of
                  Nothing ->
                    -- TODO (error): emit a warning
                    bindingTimeEnv
                  Just bityVoid ->
                    Env.addVal
                      x
                      (BTValBuiltInFixed1 varVal bityVoid)
                      bindingTimeEnv
          AssPersEntry aPtye AssPersMetadata {assPsurfaceName} ->
            let x =
                  -- Uses the same name if not specified:
                  fromMaybe varVal assPsurfaceName
             in case fromStagedPers aPtye of
                  Nothing ->
                    -- TODO (error): emit a warning
                    bindingTimeEnv
                  Just bipty ->
                    Env.addVal
                      x
                      (BTValBuiltInPersistent varVal bipty)
                      bindingTimeEnv
    )
    ( \tyName tyEntry bindingTimeEnv ->
        case tyEntry of
          Ass1TypeEntry a1tyParams a1tyeBody ->
            let r = do
                  (btTy1ParamAcc, vars, _) <-
                    foldM
                      ( \(btTy1ParamAcc', vars', i) a1tyParam ->
                          case a1tyParam of
                            A1TypeParamType atyvar -> do
                              let btvar = BITypeBoundVar i
                              pure (BITypeParamType btvar : btTy1ParamAcc', Map.insert atyvar btvar vars', i + 1)
                            A1TypeParamVal0 _ax a0tye -> do
                              btpty <- fromStaged0 a0tye
                              pure (BITypeParamVal0 btpty : btTy1ParamAcc', vars', i + 1)
                      )
                      ([], Map.empty, 0)
                      a1tyParams
                  let btTy1Params = reverse btTy1ParamAcc
                  biptyBody <- fromStaged1Body vars a1tyeBody
                  pure (btTy1Params, biptyBody)
             in case r of
                  Nothing ->
                    -- TODO (error): emit a warning
                    bindingTimeEnv
                  Just (btTy1Params, biptyBody) ->
                    Env.addType
                      tyName
                      (BTType1 (BIParameterizedType btTy1Params biptyBody))
                      bindingTimeEnv
    )
    ( \m (ModuleEntry sigr) bindingTimeEnv ->
        -- Reuses the module name `m` in the core language for the surface language:
        Env.addModule
          m
          (BTModule (makeBindingTimeEnvFromStub sigr))
          bindingTimeEnv
    )
    Env.empty
