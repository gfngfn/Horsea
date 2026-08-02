module Staged.Typechecker.Merging
  ( mergeResultsByConditional0,
    mergeTypesByConditional0,
    mergeTypesByConditional1,
  )
where

import Common.TokenUtil (Span)
import Control.Monad
{- import Data.Functor.Identity -}
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Util qualified as NonEmptyUtil
import Data.List.TwoOrMore (TwoOrMore)
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Tuple.Extra (second)
{- import Staged.Core -}
import Staged.Subst
import Staged.Syntax
import Staged.TypeError
import Staged.TypeSubst
import Staged.Typechecker.Monad
import Prelude

extractListLiteralsIfAll :: NonEmpty (Ass0Pattern, Ass0Expr) -> Maybe (NonEmpty (Ass0Pattern, [Ass0Expr]))
extractListLiteralsIfAll =
  mapM
    ( \(a0pat, a0e) ->
        case a0e of
          A0Literal (ALitList a0es) -> pure (a0pat, a0es)
          _ -> Nothing
    )

-- Performs the following conversion (i.e., transposition):
--
-- [ (p1, [e11, ..., e1N]),          [ (p1, e11),  [ (p1, e12),       [ (p1, e1N),
--   ...                      ---->    ...           ...                ...
--   (pM, [eM1, ..., eMN]) ]           (pM, eM1) ],  (pM, eM2) ], ...   (pM, eMN) ]
distribute :: NonEmpty (Ass0Pattern, [a]) -> Maybe [NonEmpty (Ass0Pattern, a)]
distribute patAndExprPairs = do
  let matrix = fmap (\(p, a0es) -> map (p,) a0es) patAndExprPairs
  NonEmptyUtil.transpose matrix

distributeTwoOrMore :: NonEmpty (Ass0Pattern, TwoOrMore a) -> Maybe (TwoOrMore (NonEmpty (Ass0Pattern, a)))
distributeTwoOrMore patAndExprPairs = do
  let matrix = fmap (\(p, a0es) -> fmap (p,) a0es) patAndExprPairs
  TwoOrMore.transpose matrix

distributeMaps :: (Ord label) => NonEmpty (pat, Map label a) -> Maybe (Map label (NonEmpty (pat, a)))
distributeMaps pairs@((_, aXrty1) :| pairsRest) =
  if all (\(_, aXrty) -> Map.keysSet aXrty == labels) pairsRest
    then
      Just $
        foldl'
          ( \acc label ->
              Map.insert label (fmap (extract label) pairs) acc
          )
          Map.empty
          labels
    else
      Nothing
  where
    labels = Map.keysSet aXrty1

    extract label (a0pat, aXrty) =
      case Map.lookup label aXrty of
        Just aXtye -> (a0pat, aXtye)
        Nothing -> error "Bug: distributeMaps"

mergeResultsByConditional0 :: forall trav. trav -> Span -> Ass0Expr -> NonEmpty (Ass0Pattern, Result0) -> M trav Result0
mergeResultsByConditional0 trav loc a0e0 = go
  where
    go :: NonEmpty (Ass0Pattern, Result0) -> M trav Result0
    go patAndResultPairs@((a0pat1, result1) :| rest) = do
      spanInFile <- askSpanInFile loc
      let failure = typeError trav $ CannotMergeResultsByConditionals spanInFile patAndResultPairs
      case result1 of
        Pure a0tye1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    Pure a0tye -> pure (a0pat, a0tye)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a0tye1) :| pairsRest
          Pure <$> mergeTypes0 pairs
        Cast0 cast1 a0tye1 r1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    Cast0 cast a0tye r -> pure (a0pat, (cast, a0tye, r))
                    _ -> failure
              )
              rest
          let quads = (a0pat1, (cast1, a0tye1, r1)) :| quadsRest
          a0tye' <- mergeTypes0 (fmap (second (\(_, a0tye, _) -> a0tye)) quads)
          cast' <- mergeCasts (fmap (second (\(cast, a0tye, _) -> (cast, a0tye))) quads)
          Cast0 cast' a0tye' <$> go (fmap (second (\(_, _, r) -> r)) quads)
        Cast1 cast1 a1tye1 r1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    Cast1 cast a1tye r -> pure (a0pat, (cast, a1tye, r))
                    _ -> failure
              )
              rest
          let quads = (a0pat1, (cast1, a1tye1, r1)) :| quadsRest
          a1tye' <- mergeTypes1 (fmap (second (\(_, a1tye, _) -> a1tye)) quads)
          cast' <- mergeCasts (fmap (second (\(cast, a1tye, _) -> (cast, A0TyCode a1tye))) quads)
          Cast1 cast' a1tye' <$> go (fmap (second (\(_, _, r) -> r)) quads)
        CastOmsGiven0 cast1 a0tye1 r1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    CastOmsGiven0 cast a0tye r -> pure (a0pat, (cast, a0tye, r))
                    _ -> failure
              )
              rest
          let quads = (a0pat1, (cast1, a0tye1, r1)) :| quadsRest
          a0tye' <- mergeTypes0 (fmap (second (\(_, a0tye, _) -> a0tye)) quads)
          cast' <- mergeCasts (fmap (second (\(cast, a0tye, _) -> (cast, a0tye))) quads)
          CastOmsGiven0 cast' a0tye' <$> go (fmap (second (\(_, _, r) -> r)) quads)
        InsertOmitted0 r1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    InsertOmitted0 r -> pure (a0pat, r)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, r1) :| pairsRest
          InsertOmitted0 <$> go pairs
        CastOmsGiven1 cast1 a1tye1 r1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    CastOmsGiven1 cast a1tye r -> pure (a0pat, (cast, a1tye, r))
                    _ -> failure
              )
              rest
          let quads = (a0pat1, (cast1, a1tye1, r1)) :| quadsRest
          a1tye' <- mergeTypes1 (fmap (second (\(_, a1tye, _) -> a1tye)) quads)
          cast' <- mergeCasts (fmap (second (\(cast, a1tye, _) -> (cast, A0TyCode a1tye))) quads)
          CastOmsGiven1 cast' a1tye' <$> go (fmap (second (\(_, _, r) -> r)) quads)
        InsertOmitted1 r1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    InsertOmitted1 r -> pure (a0pat, r)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, r1) :| pairsRest
          InsertOmitted1 <$> go pairs
        CastInfGiven0 cast1 a0tye1 r1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    CastInfGiven0 cast a0tye r -> pure (a0pat, (cast, a0tye, r))
                    _ -> failure
              )
              rest
          let quads = (a0pat1, (cast1, a0tye1, r1)) :| quadsRest
          a0tye' <- mergeTypes0 (fmap (second (\(_, a0tye, _) -> a0tye)) quads)
          cast' <- mergeCasts (fmap (second (\(cast, a0tye, _) -> (cast, a0tye))) quads)
          CastInfGiven0 cast' a0tye' <$> go (fmap (second (\(_, _, r) -> r)) quads)
        FillInferred0 a0e1 r1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    FillInferred0 a0e r -> pure (a0pat, (a0e, r))
                    _ -> failure
              )
              rest
          let triples = (a0pat1, (a0e1, r1)) :| triplesRest
          let a0branches = fmap (\(a0pat, (a0e, _)) -> A0Branch a0pat a0e) triples
          FillInferred0 (A0Case a0e0 a0branches) <$> go (fmap (second snd) triples)
        InsertInferred0 a0e1 r1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    InsertInferred0 a0e r -> pure (a0pat, (a0e, r))
                    _ -> failure
              )
              rest
          let triples = (a0pat1, (a0e1, r1)) :| triplesRest
          let a0branches = fmap (\(a0pat, (a0e, _)) -> A0Branch a0pat a0e) triples
          InsertInferred0 (A0Case a0e0 a0branches) <$> go (fmap (second snd) triples)
        Instantiated0 r1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    Instantiated0 r -> pure (a0pat, r)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, r1) :| pairsRest
          Instantiated0 <$> go pairs
        InsertInferredType0 a0tye1 r1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    InsertInferredType0 a0tye r -> pure (a0pat, (a0tye, r))
                    _ -> failure
              )
              rest
          let triples = (a0pat1, (a0tye1, r1)) :| triplesRest
          a0tye' <- mergeTypes0 (fmap (second fst) triples)
          InsertInferredType0 a0tye' <$> go (fmap (second snd) triples)
        Instantiated1 r1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    Instantiated1 r -> pure (a0pat, r)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, r1) :| pairsRest
          Instantiated1 <$> go pairs
        InsertInferredType1 a1tye1 r1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, result) ->
                  case result of
                    InsertInferredType1 a1tye r -> pure (a0pat, (a1tye, r))
                    _ -> failure
              )
              rest
          let triples = (a0pat1, (a1tye1, r1)) :| triplesRest
          a1tye' <- mergeTypes1 (fmap (second fst) triples)
          InsertInferredType1 a1tye' <$> go (fmap (second snd) triples)

    mergeTypes0 :: NonEmpty (Ass0Pattern, Ass0TypeExpr) -> M trav Ass0TypeExpr
    mergeTypes0 pairs = do
      TypecheckConfig {distributeIfUnderTensorShape} <- askConfig
      spanInFile <- askSpanInFile loc
      mapTypeError (CannotMergeTypesByConditional0 spanInFile pairs) $
        mergeTypesByConditional0 trav distributeIfUnderTensorShape a0e0 pairs

    mergeTypes1 :: NonEmpty (Ass0Pattern, Ass1TypeExpr) -> M trav Ass1TypeExpr
    mergeTypes1 pairs = do
      TypecheckConfig {distributeIfUnderTensorShape} <- askConfig
      spanInFile <- askSpanInFile loc
      mapTypeError (CannotMergeTypesByConditional1 spanInFile pairs) $
        mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0 pairs

    mergeCasts :: NonEmpty (Ass0Pattern, (Maybe Ass0Expr, Ass0TypeExpr)) -> M trav (Maybe Ass0Expr)
    mergeCasts triples =
      if all (\(_, (cast, _)) -> isNothing cast) triples
        then
          pure Nothing
        else do
          a0branches <-
            mapM
              ( \(a0pat, (cast, a0tye)) ->
                  A0Branch a0pat
                    <$> case cast of
                      Nothing -> makeIdentityLam a0tye
                      Just a0e -> pure a0e
              )
              triples
          pure $ Just (A0Case a0e0 a0branches)

mergeTypesByConditional0 :: forall trav. trav -> Bool -> Ass0Expr -> NonEmpty (Ass0Pattern, Ass0TypeExpr) -> M' ConditionalMergeError trav Ass0TypeExpr
mergeTypesByConditional0 trav distributeIfUnderTensorShape a0e0 = go0
  where
    go0 :: NonEmpty (Ass0Pattern, Ass0TypeExpr) -> M' ConditionalMergeError trav Ass0TypeExpr
    go0 patAndTypePairs@((a0pat1, a0tye1) :| rest) = do
      let failure = typeError trav $ CannotMerge0 patAndTypePairs
      case a0tye1 of
        A0TyPrim a0tyePrim1 maybePred1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyPrim a0tyePrim maybePred ->
                      if a0tyePrim == a0tyePrim1
                        then pure (a0pat, maybePred)
                        else failure
                    _ ->
                      failure
              )
              rest
          let pairs = (a0pat1, maybePred1) :| pairsRest
          maybePred' <- mergeRefinementPredicates (SA0TyPrim a0tyePrim1) pairs
          pure $ A0TyPrim a0tyePrim1 maybePred'
        A0TyList a0tyeElem1 maybePred1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyList a0tyeElem maybePred -> pure (a0pat, (a0tyeElem, maybePred))
                    _ -> failure
              )
              rest
          let triples = (a0pat1, (a0tyeElem1, maybePred1)) :| triplesRest
          a0tye' <- go0 (fmap (second fst) triples)
          maybePred' <- mergeRefinementPredicates (SA0TyList (strictify a0tyeElem1)) (fmap (second snd) triples)
          pure $ A0TyList a0tye' maybePred'
        A0TyMaybe a0tyeElem1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyMaybe a0tyeElem -> pure (a0pat, a0tyeElem)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a0tyeElem1) :| pairsRest
          A0TyMaybe <$> go0 pairs
        A0TyArrow labelOpt1 (xOpt1, a0tyeDom1) a0tyeCod1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyArrow labelOpt (xOpt, a0tyeDom) a0tyeCod ->
                      if labelOpt == labelOpt1
                        then pure (a0pat, (xOpt, a0tyeDom, a0tyeCod))
                        else failure
                    _ ->
                      failure
              )
              rest
          let quads = (a0pat1, (xOpt1, a0tyeDom1, a0tyeCod1)) :| quadsRest
          a0tyeDom' <- go0 (fmap (second (\(_, a0tyeDom, _) -> a0tyeDom)) quads)
          (xOpt', pairsForCod) <-
            if all (\(_, (xOpt, _, _)) -> isNothing xOpt) quads
              then
                pure (Nothing, fmap (second (\(_, _, a0tyeCod) -> a0tyeCod)) quads)
              else do
                ax' <- AssVarStatic <$> generateFreshVar Nothing
                let pair =
                      fmap
                        ( second
                            ( \(xOpt, _, a0tyeCod) ->
                                case xOpt of
                                  Nothing -> a0tyeCod
                                  Just x -> subst0 (A0Var ax') x a0tyeCod
                            )
                        )
                        quads
                pure (Just ax', pair)
          a0tyeCod' <- go0 pairsForCod
          pure $ A0TyArrow labelOpt1 (xOpt', a0tyeDom') a0tyeCod'
        A0TyCode a1tye1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyCode a1tye -> pure (a0pat, a1tye)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a1tye1) :| pairsRest
          A0TyCode <$> go1 pairs
        A0TyOmsArrow label1 (x1opt, a0tyeDom1) a0tyeCod1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyOmsArrow label (xOpt, a0tyeDom) a0tyeCod ->
                      if label == label1
                        then pure (a0pat, (xOpt, a0tyeDom, a0tyeCod))
                        else failure
                    _ ->
                      failure
              )
              rest
          let quads = (a0pat1, (x1opt, a0tyeDom1, a0tyeCod1)) :| quadsRest
          a0tyeDom' <- go0 (fmap (second (\(_, a0tyeDom, _) -> a0tyeDom)) quads)
          (xOpt', pairsForCod) <-
            if all (\(_, (xOpt, _, _)) -> isNothing xOpt) quads
              then
                pure (Nothing, fmap (second (\(_, _, a0tyeCod) -> a0tyeCod)) quads)
              else do
                ax' <- AssVarStatic <$> generateFreshVar Nothing
                let pair =
                      fmap
                        ( second
                            ( \(xOpt, _, a0tyeCod) ->
                                case xOpt of
                                  Nothing -> a0tyeCod
                                  Just x -> subst0 (A0Var ax') x a0tyeCod
                            )
                        )
                        quads
                pure (Just ax', pair)
          a0tyeCod' <- go0 pairsForCod
          pure $ A0TyOmsArrow label1 (xOpt', a0tyeDom') a0tyeCod'
        A0TyInfArrow (x1, a0tyeDom1) a0tyeCod1 -> do
          quadsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyInfArrow (x, a0tyeDom) a0tyeCod -> pure (a0pat, (x, a0tyeDom, a0tyeCod))
                    _ -> failure
              )
              rest
          let quads = (a0pat1, (x1, a0tyeDom1, a0tyeCod1)) :| quadsRest
          a0tyeDom' <- go0 (fmap (second (\(_, a0tyeDom, _) -> a0tyeDom)) quads)
          a0tyeCod' <- go0 ((a0pat1, a0tyeCod1) :| map (second (\(x, _, a0tyeCod) -> subst0 (A0Var x1) x a0tyeCod)) quadsRest)
          pure $ A0TyInfArrow (x1, a0tyeDom') a0tyeCod'
        A0TyProduct a0tyes1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyProduct a0tyes -> pure (a0pat, a0tyes)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a0tyes1) :| pairsRest
          case distributeTwoOrMore pairs of
            Just zipped -> do
              a0tyes' <- mapM go0 zipped
              pure $ A0TyProduct a0tyes'
            Nothing ->
              failure
        A0TyRecord a0rty1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyRecord a0rty -> pure (a0pat, a0rty)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a0rty1) :| pairsRest
          case distributeMaps pairs of
            Just zipped -> do
              a0rty' <- mapM go0 zipped
              pure $ A0TyRecord a0rty'
            Nothing ->
              failure
        A0TyVar atyvar1 -> do
          mapM_
            ( \(_a0pat, a0tye) ->
                case a0tye of
                  A0TyVar atyvar -> unless (atyvar == atyvar1) failure
                  _ -> failure
            )
            rest
          pure $ A0TyVar atyvar1
        A0TyForAll atyvar1 a0tyeSub1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, a0tye) ->
                  case a0tye of
                    A0TyForAll atyvar a0tyeSub -> pure (a0pat, (atyvar, a0tyeSub))
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a0tyeSub1) :| map (second (uncurry (tySubst0 (A0TyVar atyvar1)))) triplesRest
          A0TyForAll atyvar1 <$> go0 pairs

    mergeRefinementPredicates :: (Maybe Ass0Expr -> StrictAss0TypeExpr) -> NonEmpty (Ass0Pattern, Maybe Ass0Expr) -> M' ConditionalMergeError trav (Maybe Ass0Expr)
    mergeRefinementPredicates sa0tyef patAndMaybePredPairs =
      if all (isNothing . snd) patAndMaybePredPairs
        then
          pure Nothing
        else do
          ax <- AssVarStatic <$> generateFreshVar Nothing
          let a0branches =
                fmap
                  ( \(a0pat, maybePred) ->
                      A0Branch a0pat $
                        case maybePred of
                          Nothing -> A0Literal (ALitBool True)
                          Just a0ePred -> A0App a0ePred (A0Var ax)
                  )
                  patAndMaybePredPairs
          pure $ Just (A0Lam Nothing (ax, sa0tyef Nothing) (A0Case a0e0 a0branches))

    go1 :: NonEmpty (Ass0Pattern, Ass1TypeExpr) -> M' ConditionalMergeError trav Ass1TypeExpr
    go1 = mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0

mergeTypesByConditional1 :: forall trav. trav -> Bool -> Ass0Expr -> NonEmpty (Ass0Pattern, Ass1TypeExpr) -> M' ConditionalMergeError trav Ass1TypeExpr
mergeTypesByConditional1 trav distributeIfUnderTensorShape a0e0 = go1
  where
    go1 :: NonEmpty (Ass0Pattern, Ass1TypeExpr) -> M' ConditionalMergeError trav Ass1TypeExpr
    go1 patAndTypePairs@((a0pat1, a1tye1) :| rest) = do
      let failure = typeError trav $ CannotMerge1 patAndTypePairs
      case a1tye1 of
        A1TyPrim a1tyePrim1 -> do
          A1TyPrim
            <$> case a1tyePrim1 of
              A1TyPrimBase tyPrimBase1 -> do
                mapM_
                  ( \(_a0pat, a1tye) ->
                      case a1tye of
                        A1TyPrim (A1TyPrimBase tyPrimBase) -> unless (tyPrimBase == tyPrimBase1) failure
                        _ -> failure
                  )
                  rest
                pure a1tyePrim1
              A1TyTensor a0eList1 -> do
                pairsRest <-
                  mapM
                    ( \(a0pat, a1tye) ->
                        case a1tye of
                          A1TyPrim (A1TyTensor a0eList) -> pure (a0pat, a0eList)
                          _ -> failure
                    )
                    rest
                let pairs = (a0pat1, a0eList1) :| pairsRest
                case extractListLiteralsIfAll pairs of
                  -- Slight enhancement for the argument inference:
                  Just patAndElemsPairs | distributeIfUnderTensorShape ->
                    case distribute patAndElemsPairs of
                      Just patAndElemPairss -> do
                        let a0es' = map (A0Case a0e0 . fmap (uncurry A0Branch)) patAndElemPairss
                        pure $ A1TyTensor (A0Literal (ALitList a0es'))
                      Nothing ->
                        failure
                  -- General rule:
                  _ ->
                    pure $ A1TyTensor (A0Case a0e0 (fmap (uncurry A0Branch) pairs))
              {-
                A1TyDataset dp1 -> do
                  pairsRest <-
                    mapM
                      ( \(a0pat, a1tye) ->
                          case a1tye of
                            A1TyPrim (A1TyDataset dp) -> pure (a0pat, dp)
                            _ -> failure
                      )
                      rest
                  let pairs = (a0pat1, dp1) :| pairsRest
                  let a0branchesNumTrain = fmap (\(a0pat, dp) -> A0Branch a0pat dp.numTrain) pairs
                  let a0branchesNumTest = fmap (\(a0pat, dp) -> A0Branch a0pat dp.numTest) pairs
                  let a0branchesImage = fmap (\(a0pat, dp) -> A0Branch a0pat (runIdentity dp.image)) pairs
                  let a0branchesLabel = fmap (\(a0pat, dp) -> A0Branch a0pat (runIdentity dp.label)) pairs
                  pure . A1TyDataset $
                    DatasetParam
                      { numTrain = A0Case a0e0 a0branchesNumTrain,
                        numTest = A0Case a0e0 a0branchesNumTest,
                        image = Identity (A0Case a0e0 a0branchesImage),
                        label = Identity (A0Case a0e0 a0branchesLabel)
                      }
              -}
              A1TyLstm a0eInputSize1 a0eHiddenSize1 -> do
                triplesRest <-
                  mapM
                    ( \(a0pat, a1tye) ->
                        case a1tye of
                          A1TyPrim (A1TyLstm a0eInputSize a0eHiddenSize) -> pure (a0pat, (a0eInputSize, a0eHiddenSize))
                          _ -> failure
                    )
                    rest
                let triples = (a0pat1, (a0eInputSize1, a0eHiddenSize1)) :| triplesRest
                let a0branchesInputSize = fmap (\(a0pat, pair) -> A0Branch a0pat (fst pair)) triples
                let a0branchesHiddenSize = fmap (\(a0pat, pair) -> A0Branch a0pat (snd pair)) triples
                pure $ A1TyLstm (A0Case a0e0 a0branchesInputSize) (A0Case a0e0 a0branchesHiddenSize)
              A1TyTextHelper a0eLabels1 -> do
                pairsRest <-
                  mapM
                    ( \(a0pat, a1tye) ->
                        case a1tye of
                          A1TyPrim (A1TyTextHelper a0eLabels) -> pure (a0pat, a0eLabels)
                          _ -> failure
                    )
                    rest
                let pairs = (a0pat1, a0eLabels1) :| pairsRest
                let a0branches = fmap (uncurry A0Branch) pairs
                pure $ A1TyTextHelper (A0Case a0e0 a0branches)
        A1TyList a1tyeElem1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a1tye) ->
                  case a1tye of
                    A1TyList a1tyeElem -> pure (a0pat, a1tyeElem)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a1tyeElem1) :| pairsRest
          a1tyeElem' <- go1 pairs
          pure $ A1TyList a1tyeElem'
        A1TyMaybe a1tyeElem1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a1tye) ->
                  case a1tye of
                    A1TyMaybe a1tyeElem -> pure (a0pat, a1tyeElem)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a1tyeElem1) :| pairsRest
          a1tyeElem' <- go1 pairs
          pure $ A1TyMaybe a1tyeElem'
        A1TyData _datatyId _a1datatyArgs1 ->
          error "TODO: mergeTypesByConditional1, A1TyData"
        A1TyArrow labelOpt1 a1tyeDom1 a1tyeCod1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, a1tye) ->
                  case a1tye of
                    A1TyArrow labelOpt a1tyeDom a1tyeCod ->
                      if labelOpt == labelOpt1
                        then pure (a0pat, (a1tyeDom, a1tyeCod))
                        else failure
                    _ ->
                      failure
              )
              rest
          let triples = (a0pat1, (a1tyeDom1, a1tyeCod1)) :| triplesRest
          a1tyeDom' <- go1 (fmap (second fst) triples)
          a1tyeCod' <- go1 (fmap (second snd) triples)
          pure $ A1TyArrow labelOpt1 a1tyeDom' a1tyeCod'
        A1TyOmsArrow label1 a1tyeDom1 a1tyeCod1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, a1tye) ->
                  case a1tye of
                    A1TyOmsArrow label a1tyeDom a1tyeCod ->
                      if label == label1
                        then pure (a0pat, (a1tyeDom, a1tyeCod))
                        else failure
                    _ ->
                      failure
              )
              rest
          let triples = (a0pat1, (a1tyeDom1, a1tyeCod1)) :| triplesRest
          a1tyeDom' <- go1 (fmap (second fst) triples)
          a1tyeCod' <- go1 (fmap (second snd) triples)
          pure $ A1TyOmsArrow label1 a1tyeDom' a1tyeCod'
        A1TyProduct a1tyes1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a1tye) ->
                  case a1tye of
                    A1TyProduct a1tyes -> pure (a0pat, a1tyes)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a1tyes1) :| pairsRest
          case distributeTwoOrMore pairs of
            Just zipped -> do
              a1tyes' <- mapM go1 zipped
              pure $ A1TyProduct a1tyes'
            Nothing ->
              failure
        A1TyRecord a1rty1 -> do
          pairsRest <-
            mapM
              ( \(a0pat, a1tye) ->
                  case a1tye of
                    A1TyRecord a1rty -> pure (a0pat, a1rty)
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a1rty1) :| pairsRest
          case distributeMaps pairs of
            Just zipped -> do
              a1rty' <- mapM go1 zipped
              pure $ A1TyRecord a1rty'
            Nothing ->
              failure
        A1TyVar atyvar1 -> do
          mapM_
            ( \(_a0pat, a1tye) ->
                case a1tye of
                  A1TyVar atyvar -> unless (atyvar == atyvar1) failure
                  _ -> failure
            )
            rest
          pure $ A1TyVar atyvar1
        A1TyForAll atyvar1 a1tyeSub1 -> do
          triplesRest <-
            mapM
              ( \(a0pat, a1tye) ->
                  case a1tye of
                    A1TyForAll atyvar a1tyeSub -> pure (a0pat, (atyvar, a1tyeSub))
                    _ -> failure
              )
              rest
          let pairs = (a0pat1, a1tyeSub1) :| map (second (uncurry (tySubst1 (A1TyVar atyvar1)))) triplesRest
          A1TyForAll atyvar1 <$> go1 pairs
