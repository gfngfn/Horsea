module Staged.Typechecker
  ( typecheckExpr0,
    typecheckExpr1,
    typecheckTypeExpr0,
    typecheckTypeExpr1,
    typecheckBind,
    typecheckBinds,
    run,
  )
where

import Common.TokenUtil (Span, mergeSpan)
import Control.Monad
import Data.Either.Extra (mapLeft, maybeToEither)
import Data.Foldable (foldrM)
import Data.Function
import Data.Functor.Identity
import Data.List (length)
import Data.List.Extra (firstJust)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Util qualified as NonEmptyUtil
import Data.List.TwoOrMore (TwoOrMore)
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.Tensor.Matrix qualified as Matrix
import Data.Tensor.Vector qualified as Vector
import Data.Text (Text)
import Data.Tuple.Extra (both, second)
import Staged.BuiltIn qualified as BuiltIn
import Staged.BuiltIn.Core
import Staged.Core
import Staged.SrcSyntax
import Staged.Subst
import Staged.Syntax
import Staged.TypeError
import Staged.TypeSubst
import Staged.Typechecker.CastInsertion
import Staged.Typechecker.Monad
import Staged.Typechecker.SigRecord (Ass0Metadata (..), Ass1Metadata (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, ValEntry (..))
import Staged.Typechecker.SigRecord qualified as SigRecord
import Staged.Typechecker.Solution
import Staged.Typechecker.TypeEnv (TypeEnv, TypeVarEntry (..))
import Staged.Typechecker.TypeEnv qualified as TypeEnv
import Prelude hiding (length)

bug :: String -> a
bug msg = error $ "bug: " ++ msg

findValVar :: trav -> Span -> [Var] -> Var -> TypeEnv -> M trav ValEntry
findValVar trav loc ms x tyEnv = do
  spanInFile <- askSpanInFile loc
  liftEither $ maybeToEither (UnboundVar spanInFile ms x, trav) $ do
    case ms of
      [] ->
        TypeEnv.findVal x tyEnv
      m : ms' -> do
        ModuleEntry sigr <- TypeEnv.findModule m tyEnv
        go sigr ms'
  where
    go :: SigRecord -> [Var] -> Maybe ValEntry
    go sigr [] =
      SigRecord.findVal x sigr
    go sigr (m : ms') = do
      ModuleEntry sigr' <- SigRecord.findModule m sigr
      go sigr' ms'

findTypeVar :: trav -> Span -> TypeVar -> TypeEnv -> M trav TypeVarEntry
findTypeVar trav loc tyvar tyEnv = do
  spanInFile <- askSpanInFile loc
  case TypeEnv.findTypeVar tyvar tyEnv of
    Nothing ->
      typeError trav $ UnboundTypeVar spanInFile tyvar
    Just tyVarEntry ->
      pure tyVarEntry

convertProductToApp :: Expr -> NonEmpty ((Span, Var), Expr) -> Expr
convertProductToApp =
  foldl'
    ( \eAcc@(Expr loc1 _) ((locOp, op), eArg@(Expr loc2 _)) ->
        Expr
          (mergeSpan loc1 loc2)
          ( App
              (Expr (mergeSpan loc1 locOp) (App (Expr locOp (Var ([], op))) Nothing eAcc))
              Nothing
              eArg
          )
    )

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

instantiateGuidedByAppContext0 :: forall trav. trav -> Span -> AppContext -> Ass0TypeExpr -> M trav Result0
instantiateGuidedByAppContext0 trav loc appCtx0 a0tye0 = do
  (result, _varSolution, _tyvar0Solution) <- go Set.empty Set.empty appCtx0 a0tye0
  pure result
  where
    go :: Set AssVar -> Set AssTypeVar -> AppContext -> Ass0TypeExpr -> M trav (Result0, VarSolution, TypeVar0Solution)
    go varsToInfer tyvars0ToInfer appCtx a0tye =
      case (appCtx, a0tye) of
        ([], _) ->
          pure (Pure a0tye, Map.empty, Map.empty)
        (AppArg0 labelOpt' a0e1' a0tye1' : appCtx', A0TyArrow labelOpt (xOpt, a0tye1) a0tye2) -> do
          if labelOpt' /= labelOpt
            then do
              spanInFile <- askSpanInFile loc
              typeError trav $ ApplicationLabelMismatch spanInFile appCtx labelOpt' labelOpt
            else do
              (cast, varSolution1, tyvar0Solution1) <-
                makeAssertiveCast trav loc varsToInfer tyvars0ToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let a0tye2s = applySolution0 varSolution1 tyvar0Solution1 a0tye2
              (result', varSolution', tyvar0Solution') <-
                case xOpt of
                  Nothing -> go varsToInfer' tyvars0ToInfer' appCtx' a0tye2s
                  Just x -> go varsToInfer' tyvars0ToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let varSolution = composeVarSolution varSolution' varSolution1
              let tyvar0Solution = composeTypeVar0Solution tyvar0Solution' tyvar0Solution1
              let a0tye1s = applySolution0 varSolution tyvar0Solution a0tye1
              let result = Cast0 (fmap (applySolution0 varSolution' tyvar0Solution') cast) a0tye1s result'
              pure (result, varSolution, tyvar0Solution)
        (appCtxEntry : appCtx', A0TyOmsArrow label (xOpt, a0tyeElem1) a0tye2) -> do
          case appCtxEntry of
            AppArgOmsGiven0 label' a0e1' a0tyeElem1' | label' == label -> do
              (cast, varSolution1, tyvar0Solution1) <-
                makeAssertiveCast trav loc varsToInfer tyvars0ToInfer a0tyeElem1' a0tyeElem1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let a0tye2s = applySolution0 varSolution1 tyvar0Solution1 a0tye2
              (result', varSolution', tyvar0Solution') <-
                go varsToInfer' tyvars0ToInfer' appCtx' $
                  case xOpt of
                    Nothing -> a0tye2s
                    Just x -> subst0 a0e1' x a0tye2s
              let varSolution = composeVarSolution varSolution' varSolution1
              let tyvar0Solution = composeTypeVar0Solution tyvar0Solution' tyvar0Solution1
              let a0tyeElem1s = applySolution0 varSolution tyvar0Solution a0tyeElem1
              let result = CastOmsGiven0 (fmap (applySolution0 varSolution' tyvar0Solution') cast) a0tyeElem1s result'
              pure (result, varSolution, tyvar0Solution)
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', varSolution', tyvar0Solution') <-
                go varsToInfer tyvars0ToInfer appCtx $
                  case xOpt of
                    Nothing -> a0tye2
                    Just x -> subst0 (A0Constructor "Nothing" []) x a0tye2
              pure (InsertOmitted0 result', varSolution', tyvar0Solution')
        (appCtxEntry : appCtx', A0TyInfArrow (x, a0tye1) a0tye2) ->
          case appCtxEntry of
            AppArgInfGiven0 a0e1' a0tye1' -> do
              (cast, varSolution1, tyvar0Solution1) <-
                makeAssertiveCast trav loc varsToInfer tyvars0ToInfer a0tye1' a0tye1
              let varsToInfer' = varsToInfer \\ Map.keysSet varSolution1
              let tyvars0ToInfer' = tyvars0ToInfer \\ Map.keysSet tyvar0Solution1
              let a0tye2s = applySolution0 varSolution1 tyvar0Solution1 a0tye2
              (result', varSolution', tyvar0Solution') <-
                go varsToInfer' tyvars0ToInfer' appCtx' (subst0 a0e1' x a0tye2s)
              let varSolution = composeVarSolution varSolution' varSolution1
              let tyvar0Solution = composeTypeVar0Solution tyvar0Solution' tyvar0Solution1
              let a0tye1s = applySolution0 varSolution tyvar0Solution a0tye1
              let result = CastInfGiven0 (fmap (applySolution0 varSolution' tyvar0Solution') cast) a0tye1s result'
              pure (result, varSolution, tyvar0Solution)
            AppArgInfOmitted0 -> do
              (result', varSolution', tyvar0Solution') <-
                go (Set.insert x varsToInfer) tyvars0ToInfer appCtx' a0tye2
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x varSolution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferImplicit spanInFile x a0tye appCtx
              (cast', _varSolution'', _tyvar0Solution'') <-
                makeAssertiveCast
                  trav
                  loc
                  Set.empty
                  Set.empty
                  a0tyeInferred
                  (applySolution0 varSolution' tyvar0Solution' a0tye1)
              let result = FillInferred0 (applyCast cast' a0eInferred) result'
              pure (result, varSolution', tyvar0Solution')
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', varSolution', tyvar0Solution') <-
                go (Set.insert x varsToInfer) tyvars0ToInfer appCtx a0tye2
              (a0eInferred, a0tyeInferred) <-
                case Map.lookup x varSolution' of
                  Just entry ->
                    pure entry
                  Nothing -> do
                    spanInFile <- askSpanInFile loc
                    typeError trav $ CannotInferImplicit spanInFile x a0tye appCtx
              (cast', _varSolution'', _tyvar0Solution'') <-
                makeAssertiveCast
                  trav
                  loc
                  Set.empty
                  Set.empty
                  a0tyeInferred
                  (applySolution0 varSolution' tyvar0Solution' a0tye1)
              pure (InsertInferred0 (applyCast cast' a0eInferred) result', varSolution', tyvar0Solution')
        (_ : _, A0TyCode a1tye) -> do
          (result', varSolution) <- instantiateGuidedByAppContext1 trav loc varsToInfer appCtx a1tye
          let tyvar0Solution = Map.empty
          result <- mapMPure (pure . A0TyCode) result'
          pure (result, varSolution, tyvar0Solution)
        (appCtxEntry : appCtx', A0TyForAll atyvar a0tye2) -> do
          case appCtxEntry of
            AppArgInfTypeGiven0 a0tye1' -> do
              (result', varSolution', tyvar0Solution') <-
                go varsToInfer tyvars0ToInfer appCtx' (tySubst0 a0tye1' atyvar a0tye2)
              pure (Instantiated0 result', varSolution', tyvar0Solution')
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', varSolution', tyvar0Solution') <-
                go varsToInfer (Set.insert atyvar tyvars0ToInfer) appCtx a0tye2
              case Map.lookup atyvar tyvar0Solution' of
                Just a0tyeInferred ->
                  pure (InsertInferredType0 a0tyeInferred result', varSolution', tyvar0Solution')
                Nothing -> do
                  spanInFile <- askSpanInFile loc
                  typeError trav $ CannotInferTypeVariableInstance0 spanInFile atyvar appCtx a0tye
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye

instantiateGuidedByAppContext1 :: forall trav. trav -> Span -> Set AssVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, VarSolution)
instantiateGuidedByAppContext1 trav loc varsToInfer0 appCtx0 a1tye0 = do
  (result, varSolution, _tyvar1Solution) <- go varsToInfer0 Set.empty appCtx0 a1tye0
  pure (result, varSolution)
  where
    go :: Set AssVar -> Set AssTypeVar -> AppContext -> Ass1TypeExpr -> M trav (Result1, VarSolution, TypeVar1Solution)
    go varsToInfer tyvars1ToInfer appCtx a1tye =
      case (appCtx, a1tye) of
        ([], _) ->
          pure (Pure a1tye, Map.empty, Map.empty)
        (_ : _, A1TyForAll atyvar a1tye2) -> do
          (result', varSolution', tyvar1Solution') <-
            go varsToInfer (Set.insert atyvar tyvars1ToInfer) appCtx a1tye2
          case Map.lookup atyvar tyvar1Solution' of
            Just a1tyeInferred ->
              pure (InsertInferredType1 a1tyeInferred result', varSolution', tyvar1Solution')
            Nothing -> do
              spanInFile <- askSpanInFile loc
              typeError trav $ CannotInferTypeVariableInstance1 spanInFile atyvar appCtx a1tye
        (AppArg1 labelOpt' a1tye1' : appCtx', A1TyArrow labelOpt a1tye1 a1tye2) -> do
          if labelOpt' /= labelOpt
            then do
              spanInFile <- askSpanInFile loc
              typeError trav $ ApplicationLabelMismatch spanInFile appCtx labelOpt' labelOpt
            else do
              (eq, varSolution1, tyvar1Solution1) <-
                makeEquation1 trav loc varsToInfer tyvars1ToInfer a1tye1' a1tye1
              (result', varSolution', tyvar1Solution') <-
                go
                  (varsToInfer \\ Map.keysSet varSolution1)
                  (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
                  appCtx'
                  (applySolution1 varSolution1 tyvar1Solution1 a1tye2)
              let varSolution = composeVarSolution varSolution' varSolution1
              let tyvar1Solution = composeTypeVar1Solution tyvar1Solution' tyvar1Solution1
              let result = Cast1 (fmap (applySolution1 varSolution' tyvar1Solution' . A0TyEqAssert loc) eq) a1tye1 result'
              pure (result, varSolution, tyvar1Solution)
        (appCtxEntry : appCtx', A1TyOmsArrow label a1tye1 a1tye2) ->
          case appCtxEntry of
            AppArgOmsGiven1 label' a1tye1' | label' == label -> do
              (eq, varSolution1, tyvar1Solution1) <-
                makeEquation1 trav loc varsToInfer tyvars1ToInfer a1tye1' a1tye1
              (result', varSolution', tyvar1Solution') <-
                go
                  (varsToInfer \\ Map.keysSet varSolution1)
                  (tyvars1ToInfer \\ Map.keysSet tyvar1Solution1)
                  appCtx'
                  (applySolution1 varSolution1 tyvar1Solution1 a1tye2)
              let varSolution = composeVarSolution varSolution' varSolution1
              let tyvar1Solution = composeTypeVar1Solution tyvar1Solution' tyvar1Solution1
              let result = CastOmsGiven1 (fmap (applySolution1 varSolution' tyvar1Solution' . A0TyEqAssert loc) eq) a1tye1 result'
              pure (result, varSolution, tyvar1Solution)
            _ -> do
              -- Recurses by using `appCtx`, not `appCtx'`:
              (result', varSolution', tyvar0Solution') <- go varsToInfer tyvars1ToInfer appCtx a1tye2
              pure (InsertOmitted1 result', varSolution', tyvar0Solution')
        _ -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye

forceExpr0 :: trav -> TypeEnv -> Ass0TypeExpr -> Expr -> M trav Ass0Expr
forceExpr0 trav tyEnv a0tyeReq e@(Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  case eMain of
    Literal (LitList es) ->
      case a0tyeReq of
        A0TyList a0tyeElem _maybePred -> do
          a0es <- mapM (forceExpr0 trav tyEnv a0tyeElem) es
          pure $ A0Literal (ALitList a0es)
        _ ->
          typeError trav $ CannotForceType0 spanInFile a0tyeReq
    Tuple es -> do
      case a0tyeReq of
        A0TyProduct a0tyesReq -> do
          case TwoOrMore.zipExact a0tyesReq es of
            Just zipped -> do
              a0es <- mapM (uncurry (forceExpr0 trav tyEnv)) zipped
              pure $ A0Tuple a0es
            Nothing ->
              typeError trav $ CannotForceType0 spanInFile a0tyeReq
        _ ->
          typeError trav $ CannotForceType0 spanInFile a0tyeReq
    Constructor (mods, ctor) ->
      case (mods, ctor) of
        ([], "Nothing") ->
          case a0tyeReq of
            A0TyMaybe _a0tyeElem -> pure $ A0Constructor "Nothing" []
            _ -> typeError trav $ CannotForceType0 spanInFile a0tyeReq
        _ -> do
          (a0tye, a0e) <- typecheckExpr0Single trav tyEnv e
          (cast, _varSolution, _tyvar0Solution) <- makeAssertiveCast trav loc Set.empty Set.empty a0tye a0tyeReq
          pure $ applyCast cast a0e
    IfThenElse e0 e1 e2 -> do
      (a0tye0, a0e0) <- typecheckExpr0Single trav tyEnv e0
      case a0tye0 of
        A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePred -> do
          a0e1 <- forceExpr0 trav tyEnv a0tyeReq e1
          a0e2 <- forceExpr0 trav tyEnv a0tyeReq e2
          pure $ A0IfThenElse a0e0 a0e1 a0e2
        _ -> do
          let Expr loc0 _ = e0
          spanInFile0 <- askSpanInFile loc0
          typeError trav $ NotABoolTypeForStage0 spanInFile0 a0tye0
    _ -> do
      (a0tye, a0e) <- typecheckExpr0Single trav tyEnv e
      (cast, _varSolution, _tyvar0Solution) <- makeAssertiveCast trav loc Set.empty Set.empty a0tye a0tyeReq
      pure $ applyCast cast a0e

typecheckExpr0Single :: trav -> TypeEnv -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckExpr0Single trav tyEnv e@(Expr loc _) = do
  (result0, a0e) <- typecheckExpr0 trav tyEnv [] e
  case result0 of
    Pure a0tye ->
      pure (a0tye, a0e)
    _ -> do
      spanInFile <- askSpanInFile loc
      bug $ "non-empty result0: " ++ show spanInFile

typecheckExpr0 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result0, Ass0Expr)
typecheckExpr0 trav tyEnv appCtx (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  completeImplicit spanInFile
    =<< case eMain of
      Constructor (mods, ctor) ->
        case (mods, ctor) of
          ([], "Just") ->
            case appCtx of
              [] ->
                typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
              [AppArg0 Nothing _a0e1 a0tye1] -> do
                svX <- generateFreshVar Nothing
                let ax = AssVarStatic svX
                let a0eRet = A0Lam Nothing (ax, strictify a0tye1) (A0Constructor "Just" [A0Var ax])
                pure (Cast0 Nothing a0tye1 (Pure (A0TyMaybe a0tye1)), a0eRet)
              _ ->
                error "TODO (error): other app contexts"
          ([], "Nothing") ->
            typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
          (_, _) ->
            typeError trav $ UnboundConstructor spanInFile mods ctor
      Product e1 rest ->
        typecheckExpr0 trav tyEnv appCtx (convertProductToApp e1 rest)
      Literal lit ->
        case appCtx of
          [] -> do
            (a0tye, alit) <-
              case lit of
                LitInt n ->
                  -- Ad hoc optimization about `Nat`
                  pure (if n >= 0 then BuiltIn.tyNat else A0TyPrim (A0TyPrimBase ATyPrimInt) Nothing, ALitInt n)
                LitFloat r ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimFloat) Nothing, ALitFloat r)
                LitUnit ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimUnit) Nothing, ALitUnit)
                LitBool b ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimBool) Nothing, ALitBool b)
                LitString t ->
                  pure (A0TyPrim (A0TyPrimBase ATyPrimString) Nothing, ALitString t)
                LitList es ->
                  case es of
                    [] ->
                      typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
                    eFirst : esTail -> do
                      (a0tyeFirst, a0eFirst) <- typecheckExpr0Single trav tyEnv eFirst
                      a0esTail <- mapM (forceExpr0 trav tyEnv a0tyeFirst) esTail
                      pure (A0TyList a0tyeFirst Nothing, ALitList (a0eFirst : a0esTail))
                LitVec ns -> do
                  let vec = Vector.fromList ns
                  pure (A0TyPrim (a0TyVec (Vector.length vec)) Nothing, ALitVec vec)
                LitMat nss -> do
                  mat <-
                    liftEither . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $
                      Matrix.fromRows nss
                  pure (A0TyPrim (uncurry a0TyMat (Matrix.size mat)) Nothing, ALitMat mat)
            pure (Pure a0tye, A0Literal alit)
          _ : _ ->
            typeError trav $ CannotApplyLiteral spanInFile
      Var (ms, x) -> do
        (a0tye, a0e) <- typecheckValVar0 trav loc tyEnv ms x
        result <- instantiateGuidedByAppContext0 trav loc appCtx a0tye
        pure (result, a0e)
      Lam recOpt labelOpt (x1, tye1) e2 ->
        case appCtx of
          [] -> do
            svX1 <- generateFreshVar (Just x1)
            let ax1 = AssVarStatic svX1
            case recOpt of
              Nothing -> do
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (a0tye2, a0e2) <- do
                  let tyEnv' = TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1)) tyEnv
                  typecheckExpr0Single trav tyEnv' e2
                let sa0tye1 = strictify a0tye1
                pure (Pure (A0TyArrow labelOpt (Just ax1, a0tye1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
              Just (f, tyeRec) -> do
                svF <- generateFreshVar (Just f)
                let af = AssVarStatic svF
                a0tyeRec <- typecheckTypeExpr0 trav tyEnv tyeRec
                a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
                (a0tye2, a0e2) <- do
                  let tyEnv' =
                        tyEnv
                          & TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1))
                          & TypeEnv.addVal f (Ass0Entry a0tyeRec (Right svF))
                  typecheckExpr0Single trav tyEnv' e2
                let a0tyeSynth = A0TyArrow labelOpt (Just ax1, a0tye1) a0tye2
                (cast, _varSolution, _tyvar0Solution) <-
                  makeAssertiveCast trav loc Set.empty Set.empty a0tyeSynth a0tyeRec
                let sa0tyeRec = strictify a0tyeRec
                let sa0tye1 = strictify a0tye1
                pure (Pure a0tyeRec, applyCast cast (A0Lam (Just (af, sa0tyeRec)) (ax1, sa0tye1) a0e2))
          _ : _ ->
            -- TODO (enhance): consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamWithArguments appCtx
      App e1 labelOpt e2 -> do
        (a0tye2, a0e2) <- typecheckExpr0Single trav tyEnv e2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArg0 labelOpt a0e2 a0tye2 : appCtx) e1
        case result1 of
          Cast0 cast _a0tye11 result -> do
            pure (result, A0App a0e1 (applyCast cast a0e2))
          _ -> do
            bug "stage-0, App, fun"
      LamOms label (x1, tye1@(Expr locTye1 _)) e2 -> do
        svX1 <- generateFreshVar (Just x1)
        let ax1 = AssVarStatic svX1
        case appCtx of
          [] -> do
            a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
            case a0tye1 of
              A0TyMaybe a0tyeElem1 -> do
                (a0tye2, a0e2) <- do
                  let tyEnv' = TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1)) tyEnv
                  typecheckExpr0Single trav tyEnv' e2
                let sa0tye1 = strictify a0tye1
                pure (Pure (A0TyOmsArrow label (Just ax1, a0tyeElem1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
              _ -> do
                spanInFile1 <- askSpanInFile locTye1
                typeError trav $ NonMaybeAnnotForLamOms0 spanInFile1 a0tye1
          _ : _ ->
            -- TODO (enhance): consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamOmsWithArguments appCtx
      AppOms e1 label e2 -> do
        (a0tye2, a0e2) <- typecheckExpr0Single trav tyEnv e2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgOmsGiven0 label a0e2 a0tye2 : appCtx) e1
        case result1 of
          CastOmsGiven0 cast _a0tyeElem11 result -> do
            pure (result, A0App a0e1 (A0Constructor "Just" [applyCast cast a0e2]))
          _ ->
            bug "stage-0, AppOms, fun"
      LamInf (x1, tye1) e2 -> do
        svX1 <- generateFreshVar (Just x1)
        let ax1 = AssVarStatic svX1
        case appCtx of
          [] -> do
            a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
            (a0tye2, a0e2) <- do
              let tyEnv' = TypeEnv.addVal x1 (Ass0Entry a0tye1 (Right svX1)) tyEnv
              typecheckExpr0Single trav tyEnv' e2
            let sa0tye1 = strictify a0tye1
            pure (Pure (A0TyInfArrow (ax1, a0tye1) a0tye2), A0Lam Nothing (ax1, sa0tye1) a0e2)
          _ : _ ->
            -- TODO (enhance): consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamInfWithArguments appCtx
      AppInfGiven e1 e2 -> do
        (a0tye2, a0e2) <- typecheckExpr0Single trav tyEnv e2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgInfGiven0 a0e2 a0tye2 : appCtx) e1
        case result1 of
          CastInfGiven0 cast _a0tye11 result -> do
            logInferableArg $ LogGivenArg spanInFile a0e2
            pure (result, A0App a0e1 (applyCast cast a0e2))
          _ -> do
            bug "stage-0, AppImpGiven, not a CastGiven0"
      AppInfOmitted e1 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgInfOmitted0 : appCtx) e1
        case result1 of
          FillInferred0 a0eInferred result -> do
            logInferableArg $ LogInferredArg spanInFile a0eInferred
            pure (result, A0App a0e1 a0eInferred)
          _ -> do
            bug "stage-0, AppImpOmitted, not a FillInferred0"
      LetIn x params tyeBodyOpt e1 e2 -> do
        svX <- generateFreshVar (Just x)
        let ax = AssVarStatic svX
        (a0tye1, a0e1) <- typecheckLetInBody0 trav tyEnv params tyeBodyOpt e1
        (result2, a0e2) <- do
          let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
          typecheckExpr0 trav tyEnv' appCtx e2
        let sa0tye1 = strictify a0tye1
        if ax `occurs0` result2
          then typeError trav $ VarOccursFreelyInAss0Type spanInFile x result2
          else pure (result2, A0LetIn (ax, sa0tye1) a0e1 a0e2)
      LetRecIn f params tyeBody eBody e2 -> do
        a0tye1Rec <- constructFunTypeExpr0 trav tyEnv params tyeBody
        (labelOpt, x0, tyeParam0, paramsRest) <-
          case params of
            MandatoryBinder labelOpt' (x0', tyeParam0') : paramsRest' ->
              pure (labelOpt', x0', tyeParam0', paramsRest')
            OmissibleBinder {} : _ ->
              typeError trav $ LetRecParamsCannotStartWithImplicit spanInFile
            InferableBinder {} : _ ->
              typeError trav $ LetRecParamsCannotStartWithImplicit spanInFile
            TypeBinder {} : _ ->
              error "TODO: typecheckExpr0, LetRecIn, TypeBinder"
            [] ->
              typeError trav $ LetRecRequiresNonEmptyParams spanInFile
        svFInner <- generateFreshVar (Just f)
        let afInner = AssVarStatic svFInner
        svX0 <- generateFreshVar (Just x0)
        let ax0 = AssVarStatic svX0
        a0tyeParam0 <- typecheckTypeExpr0 trav tyEnv tyeParam0
        (a0tyeRestSynth, a0eRest) <- do
          let tyEnv' =
                tyEnv
                  & TypeEnv.addVal f (Ass0Entry a0tye1Rec (Right svFInner))
                  & TypeEnv.addVal x0 (Ass0Entry a0tyeParam0 (Right svX0))
          typecheckLetInBody0 trav tyEnv' paramsRest (Just tyeBody) eBody
        let a0tye1Synth = A0TyArrow labelOpt (Just ax0, a0tyeParam0) a0tyeRestSynth
        (cast, _varSolution, _tyvar0Solution) <-
          makeAssertiveCast trav loc Set.empty Set.empty a0tye1Synth a0tye1Rec
        let a0e1 = applyCast cast (A0Lam (Just (afInner, strictify a0tye1Rec)) (ax0, strictify a0tyeParam0) a0eRest)
        svFOuter <- generateFreshVar (Just f)
        let afOuter = AssVarStatic svFOuter
        (result2, a0e2) <- do
          let tyEnv' = TypeEnv.addVal f (Ass0Entry a0tye1Rec (Right svFOuter)) tyEnv
          typecheckExpr0 trav tyEnv' appCtx e2
        if afOuter `occurs0` result2
          then
            typeError trav $ VarOccursFreelyInAss0Type spanInFile f result2
          else do
            pure (result2, A0LetIn (afOuter, strictify a0tye1Rec) a0e1 a0e2)
      LetTupleIn xs e1@(Expr loc1 _) e2 -> do
        (a0tye1, a0e1) <- typecheckExpr0Single trav tyEnv e1
        case a0tye1 of
          A0TyProduct a0tyes -> do
            zipped <-
              case TwoOrMore.zipExact xs a0tyes of
                Just zipped' ->
                  pure zipped'
                Nothing -> do
                  spanInFile1 <- askSpanInFile loc1
                  typeError trav $ LetTupleLengthMismatch0 spanInFile1 xs a0tyes
            triples <-
              mapM
                ( \(x, a0tye) -> do
                    svX <- generateFreshVar (Just x)
                    pure ((x, a0tye), svX)
                )
                zipped
            (result2, a0e2) <- do
              let tyEnv2 =
                    foldl'
                      ( \tyEnv' ((x, a0tye), svX) ->
                          TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv'
                      )
                      tyEnv
                      triples
              typecheckExpr0 trav tyEnv2 appCtx e2
            pure (result2, A0LetTupleIn (fmap (AssVarStatic . snd) triples) a0e1 a0e2)
          _ -> do
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotATupleAtStage0 spanInFile1 a0tye1
      LetOpenIn m e -> do
        case TypeEnv.findModule m tyEnv of
          Nothing ->
            typeError trav $ UnboundModule spanInFile m
          Just (ModuleEntry sigr) -> do
            let tyEnv' = TypeEnv.appendSigRecord tyEnv sigr
            typecheckExpr0 trav tyEnv' appCtx e
      Sequential e1 e2 -> do
        (a0tye1, a0e1) <- typecheckExpr0Single trav tyEnv e1
        case a0tye1 of
          A0TyPrim (A0TyPrimBase ATyPrimUnit) _maybePred -> do
            (result2, a0e2) <- typecheckExpr0 trav tyEnv appCtx e2
            pure (result2, A0Sequential a0e1 a0e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotAUnitTypeForStage0 spanInFile1 a0tye1
      Tuple es -> do
        case appCtx of
          [] -> do
            pairs <- mapM (typecheckExpr0Single trav tyEnv) es
            pure (Pure (A0TyProduct (fmap fst pairs)), A0Tuple (fmap snd pairs))
          _ : _ -> do
            typeError trav $ CannotApplyTuple spanInFile
      IfThenElse e0 e1 e2 -> do
        (a0tye0, a0e0) <- typecheckExpr0Single trav tyEnv e0
        case a0tye0 of
          A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePred -> do
            (result1, a0e1) <- typecheckExpr0 trav tyEnv appCtx e1
            (result2, a0e2) <- typecheckExpr0 trav tyEnv appCtx e2
            result <-
              mergeResultsByConditional0 trav loc a0e0 $
                (A0PatBool True, result1) :| [(A0PatBool False, result2)]
            pure (result, A0IfThenElse a0e0 a0e1 a0e2)
          _ -> do
            let Expr loc0 _ = e0
            spanInFile0 <- askSpanInFile loc0
            typeError trav $ NotABoolTypeForStage0 spanInFile0 a0tye0
      Case e0 branches -> do
        (a0tye0, a0e0) <- typecheckExpr0Single trav tyEnv e0
        triples <- mapM (forceBranch0 trav tyEnv a0tye0 appCtx) branches
        result' <- mergeResultsByConditional0 trav loc a0e0 $ fmap (second fst) triples
        let a0branches = fmap (\(a0pat, (_, a0eRet)) -> A0Branch a0pat a0eRet) triples
        pure (result', A0Case a0e0 a0branches)
      As e1 tye2 ->
        case appCtx of
          [] -> do
            a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
            a0e1 <- forceExpr0 trav tyEnv a0tye2 e1
            pure (Pure a0tye2, a0e1)
          _ : _ ->
            typeError trav $ Unsupported spanInFile $ AsWithArguments appCtx
      Bracket e1 -> do
        (result1, a1e1) <- typecheckExpr1 trav tyEnv appCtx e1
        result <- mapMPure (pure . A0TyCode) result1
        pure (result, A0Bracket a1e1)
      Escape _ ->
        typeError trav $ CannotUseEscapeAtStage0 spanInFile
      LamInfType tyvar1 e2 ->
        case appCtx of
          [] -> do
            atyvar1 <- generateFreshTypeVar tyvar1
            (a0tye2, a0e2) <- do
              let tyEnv' = TypeEnv.addTypeVar tyvar1 (TypeVarEntry0 atyvar1) tyEnv
              typecheckExpr0Single trav tyEnv' e2
            pure (Pure (A0TyForAll atyvar1 a0tye2), A0LamType atyvar1 a0e2)
          _ : _ ->
            error "TODO: typecheckExpr0, LamInfType, non-empty context"
      AppInfType e1 tye2 -> do
        a0tye2 <- typecheckTypeExpr0 trav tyEnv tye2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArgInfTypeGiven0 a0tye2 : appCtx) e1
        case result1 of
          Instantiated0 result -> do
            pure (result, A0AppType a0e1 (strictify a0tye2))
          _ -> do
            bug "stage-0, AppInfType"
      Persistent _ ->
        typeError trav $ CannotUsePersistent spanInFile
      (TyVar {}; TyArrow {}; TyOmsArrow {}; TyInfArrow {}; TyRefinement {}; TyForAll {}) ->
        typeError trav $ InvalidSyntaxAsExpr spanInFile
  where
    completeImplicit spanInFile = go
      where
        go pair@(result, a0e) =
          case result of
            InsertOmitted0 result' ->
              go (result', A0App a0e (A0Constructor "Nothing" []))
            InsertInferred0 a0eInferred result' -> do
              logInferableArg $ LogInferredArg spanInFile a0eInferred
              go (result', A0App a0e a0eInferred)
            InsertInferredType0 a0tyeInferred result' ->
              go (result', A0AppType a0e (strictify a0tyeInferred))
            _ ->
              pure pair

forceBranch0 :: trav -> TypeEnv -> Ass0TypeExpr -> AppContext -> Branch -> M trav (Ass0Pattern, (Result0, Ass0Expr))
forceBranch0 trav tyEnv a0tyePatReq appCtx (Branch pat e) = do
  (a0pat, binders) <- forcePattern0 trav tyEnv a0tyePatReq pat
  (result, a0e) <- typecheckExpr0 trav (TypeEnv.addVals binders tyEnv) appCtx e
  pure (a0pat, (result, a0e))

forceBranch1 :: trav -> TypeEnv -> Ass1TypeExpr -> Branch -> M trav (Ass1Pattern, (Ass1TypeExpr, Ass1Expr))
forceBranch1 trav tyEnv a1tyePatReq (Branch pat e) = do
  (a1pat, binders) <- forcePattern1 trav tyEnv a1tyePatReq pat
  (a1tye, a1e) <- typecheckExpr1Single trav (TypeEnv.addVals binders tyEnv) e
  pure (a1pat, (a1tye, a1e))

collectPatternArgs :: trav -> Span -> PatternMain -> M trav (([ModuleName], ConstructorName), [Pattern])
collectPatternArgs trav loc = \case
  PatConstructor (mods, ctor) ->
    pure ((mods, ctor), [])
  PatApp (Pattern loc1 patMain1) pat2 -> do
    (qualCtor, patArgs1) <- collectPatternArgs trav loc1 patMain1
    pure (qualCtor, patArgs1 ++ [pat2])
  (PatBool _; PatVar _; PatListNil) -> do
    spanInFile <- askSpanInFile loc
    typeError trav $ InvalidSyntaxAsPattern spanInFile

forcePattern0 :: trav -> TypeEnv -> Ass0TypeExpr -> Pattern -> M trav (Ass0Pattern, Map Var ValEntry)
forcePattern0 trav tyEnv a0tyePatReq (Pattern loc patMain) = do
  spanInFile <- askSpanInFile loc
  case patMain of
    PatConstructor (mods, ctor) ->
      case (mods, ctor) of
        ([], "Nothing") ->
          case a0tyePatReq of
            A0TyMaybe _ -> pure (A0PatConstructor "Nothing" [], Map.empty)
            _ -> typeError trav $ CannotForceTypeOnPattern0 spanInFile a0tyePatReq
        (_, _) ->
          typeError trav $ UnboundConstructorOrInvalidArity spanInFile mods ctor 0
    PatApp _ _ -> do
      ((mods, ctor), patArgs) <- collectPatternArgs trav loc patMain
      case (mods, ctor, patArgs) of
        ([], "Just", [pat1]) ->
          case a0tyePatReq of
            A0TyMaybe a0tyePatReq1 -> do
              (a0pat1, binders) <- forcePattern0 trav tyEnv a0tyePatReq1 pat1
              pure (A0PatConstructor "Just" [a0pat1], binders)
            _ ->
              typeError trav $ CannotForceTypeOnPattern0 spanInFile a0tyePatReq
        ([], "::", [pat1, pat2]) ->
          case a0tyePatReq of
            A0TyList a0tyePatElemReq _maybePred -> do
              (a0pat1, binders1) <- forcePattern0 trav tyEnv a0tyePatElemReq pat1
              (a0pat2, binders2) <- forcePattern0 trav tyEnv a0tyePatReq pat2
              binders <- disjointUnion trav binders1 binders2
              pure (A0PatListCons a0pat1 a0pat2, binders)
            _ ->
              typeError trav $ CannotForceTypeOnPattern0 spanInFile a0tyePatReq
        (_, _, _) ->
          typeError trav $ UnboundConstructorOrInvalidArity spanInFile mods ctor (length patArgs)
    PatVar x -> do
      svX <- generateFreshVar (Just x)
      let ax = AssVarStatic svX
      pure (A0PatVar ax, Map.singleton x (Ass0Entry a0tyePatReq (Right svX)))
    PatListNil ->
      case a0tyePatReq of
        A0TyList _ _maybePred -> pure (A0PatListNil, Map.empty)
        _ -> typeError trav $ CannotForceTypeOnPattern0 spanInFile a0tyePatReq
    PatBool b ->
      case a0tyePatReq of
        A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePred ->
          pure (A0PatBool b, Map.empty)
        _ ->
          typeError trav $ CannotForceTypeOnPattern0 spanInFile a0tyePatReq

-- TODO: judge that two maps are disjoint
disjointUnion :: trav -> Map Var ValEntry -> Map Var ValEntry -> M trav (Map Var ValEntry)
disjointUnion _trav binders1 binders2 =
  pure $ Map.union binders1 binders2

forcePattern1 :: trav -> TypeEnv -> Ass1TypeExpr -> Pattern -> M trav (Ass1Pattern, Map Var ValEntry)
forcePattern1 trav tyEnv a1tyePatReq (Pattern loc patMain) = do
  spanInFile <- askSpanInFile loc
  case patMain of
    PatConstructor (mods, ctor) ->
      case (mods, ctor) of
        (_, "Nothing") ->
          case a1tyePatReq of
            A1TyMaybe _ -> pure (A1PatConstructor "Nothing" [], Map.empty)
            _ -> typeError trav $ CannotForceTypeOnPattern1 spanInFile a1tyePatReq
        _ ->
          typeError trav $ UnboundConstructorOrInvalidArity spanInFile mods ctor 0
    PatApp _ _ -> do
      ((mods, ctor), patArgs) <- collectPatternArgs trav loc patMain
      case (mods, ctor, patArgs) of
        ([], "Just", [pat1]) ->
          case a1tyePatReq of
            A1TyMaybe a1tyePatReq1 -> do
              (a1pat1, binders) <- forcePattern1 trav tyEnv a1tyePatReq1 pat1
              pure (A1PatConstructor "Just" [a1pat1], binders)
            _ ->
              typeError trav $ CannotForceTypeOnPattern1 spanInFile a1tyePatReq
        ([], "::", [pat1, pat2]) ->
          case a1tyePatReq of
            A1TyList a1tyePatElemReq -> do
              (a1pat1, binders1) <- forcePattern1 trav tyEnv a1tyePatElemReq pat1
              (a1pat2, binders2) <- forcePattern1 trav tyEnv a1tyePatReq pat2
              binders <- disjointUnion trav binders1 binders2
              pure (A1PatListCons a1pat1 a1pat2, binders)
            _ ->
              typeError trav $ CannotForceTypeOnPattern1 spanInFile a1tyePatReq
        (_, _, _) ->
          typeError trav $ UnboundConstructorOrInvalidArity spanInFile mods ctor (length patArgs)
    PatVar x -> do
      svX <- generateFreshVar (Just x)
      let ax = AssVarStatic svX
      pure (A1PatVar ax, Map.singleton x (Ass1Entry a1tyePatReq (Right svX)))
    PatListNil ->
      case a1tyePatReq of
        A1TyList _ -> pure (A1PatListNil, Map.empty)
        _ -> typeError trav $ CannotForceTypeOnPattern1 spanInFile a1tyePatReq
    PatBool b ->
      case a1tyePatReq of
        A1TyPrim (A1TyPrimBase ATyPrimBool) ->
          pure (A1PatBool b, Map.empty)
        _ ->
          typeError trav $ CannotForceTypeOnPattern1 spanInFile a1tyePatReq

constructFunTypeExpr0 :: trav -> TypeEnv -> [LamBinder] -> TypeExpr -> M trav Ass0TypeExpr
constructFunTypeExpr0 trav tyEnv params tyeBody = do
  (tyEnv', f) <-
    foldM
      ( \(tyEnv0, f0) param ->
          case param of
            MandatoryBinder labelOpt (x, tye) -> do
              svX <- generateFreshVar (Just x)
              let ax = AssVarStatic svX
              a0tye <- typecheckTypeExpr0 trav tyEnv0 tye
              let tyEnv1 = TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv0
              let f1 = f0 . A0TyArrow labelOpt (Just ax, a0tye)
              pure (tyEnv1, f1)
            OmissibleBinder label (x, tye@(Expr locTye _)) -> do
              svX <- generateFreshVar (Just x)
              let ax = AssVarStatic svX
              a0tye <- typecheckTypeExpr0 trav tyEnv0 tye
              case a0tye of
                A0TyMaybe a0tyeElem -> do
                  let tyEnv1 = TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv0
                  let f1 = f0 . A0TyOmsArrow label (Just ax, a0tyeElem)
                  pure (tyEnv1, f1)
                _ -> do
                  spanInFile <- askSpanInFile locTye
                  typeError trav $ NonMaybeAnnotForLamOms0 spanInFile a0tye
            InferableBinder (x, tye) -> do
              svX <- generateFreshVar (Just x)
              let ax = AssVarStatic svX
              a0tye <- typecheckTypeExpr0 trav tyEnv0 tye
              let tyEnv1 = TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv0
              let f1 = f0 . A0TyInfArrow (ax, a0tye)
              pure (tyEnv1, f1)
            TypeBinder tyvar -> do
              atyvar <- generateFreshTypeVar tyvar
              let tyEnv1 = TypeEnv.addTypeVar tyvar (TypeVarEntry0 atyvar) tyEnv0
              let f1 = f0 . A0TyForAll atyvar
              pure (tyEnv1, f1)
      )
      (tyEnv, id)
      params
  a0tyeBody <- typecheckTypeExpr0 trav tyEnv' tyeBody
  pure $ f a0tyeBody

constructFunTypeExpr1 :: trav -> Span -> TypeEnv -> [LamBinder] -> TypeExpr -> M trav Ass1TypeExpr
constructFunTypeExpr1 trav loc tyEnv params tyeBody = do
  spanInFile <- askSpanInFile loc
  a1tyeBody <- typecheckTypeExpr1 trav tyEnv tyeBody
  foldrM
    ( \param a1tyeAcc ->
        case param of
          MandatoryBinder labelOpt (_x, tye) -> do
            a1tye <- typecheckTypeExpr1 trav tyEnv tye
            pure $ A1TyArrow labelOpt a1tye a1tyeAcc
          OmissibleBinder label (_x, tye@(Expr locTye _)) -> do
            a1tye <- typecheckTypeExpr1 trav tyEnv tye
            case a1tye of
              A1TyMaybe a1tyeElem ->
                pure $ A1TyOmsArrow label a1tyeElem a1tyeAcc
              _ -> do
                spanInFile' <- askSpanInFile locTye
                typeError trav $ NonMaybeAnnotForLamOms1 spanInFile' a1tye
          InferableBinder (_x, _tye) ->
            typeError trav $ CannotUseLamInfAtStage1 spanInFile
          TypeBinder tyvar -> do
            atyvar <- generateFreshTypeVar tyvar
            pure $ A1TyForAll atyvar a1tyeAcc
    )
    a1tyeBody
    params

typecheckValVar0 :: trav -> Span -> TypeEnv -> [ModuleName] -> Var -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckValVar0 trav loc tyEnv mods x = do
  valEntry <- findValVar trav loc mods x tyEnv
  (a0tye, builtInNameOrSv) <-
    case valEntry of
      Ass0Entry a0tye' a0metadataOrSv ->
        pure . (a0tye',) $
          case a0metadataOrSv of
            Left Ass0Metadata {ass0builtInName} -> Left ass0builtInName
            Right svx -> Right svx
      AssPersEntry aPtye AssPersMetadata {assPbuiltInName} ->
        pure (persistentTypeTo0 aPtye, Left (unliftBuiltInName assPbuiltInName))
      Ass1Entry _ _ -> do
        spanInFile <- askSpanInFile loc
        typeError trav $ NotAStage0Var spanInFile x
  pure . (a0tye,) $
    case builtInNameOrSv of
      Left builtInName -> A0BuiltInName builtInName
      Right svX -> A0Var (AssVarStatic svX)

typecheckValVar1 :: trav -> Span -> TypeEnv -> [ModuleName] -> Var -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckValVar1 trav loc tyEnv mods x = do
  valEntry <- findValVar trav loc mods x tyEnv
  (a1tye, a1builtInNameOrSv) <-
    case valEntry of
      Ass0Entry _ _ -> do
        spanInFile <- askSpanInFile loc
        typeError trav $ NotAStage1Var spanInFile x
      AssPersEntry aPtye AssPersMetadata {assPbuiltInName} ->
        pure (persistentTypeTo1 aPtye, Left assPbuiltInName)
      Ass1Entry a1tye' a1metadataOrSv ->
        pure . (a1tye',) $
          case a1metadataOrSv of
            Left Ass1Metadata {ass1builtInName} -> Left ass1builtInName
            Right svX -> Right svX
  pure . (a1tye,) $
    case a1builtInNameOrSv of
      Left a1builtInName -> A1BuiltInName a1builtInName
      Right svX -> A1Var (AssVarStatic svX)

typecheckLetInBody0 :: trav -> TypeEnv -> [LamBinder] -> Maybe TypeExpr -> Expr -> M trav (Ass0TypeExpr, Ass0Expr)
typecheckLetInBody0 trav tyEnv params tyeBodyOpt e1 =
  case params of
    [] ->
      case tyeBodyOpt of
        Just tyeBody -> do
          a0tye1req <- typecheckTypeExpr0 trav tyEnv tyeBody
          a0e1 <- forceExpr0 trav tyEnv a0tye1req e1
          pure (a0tye1req, a0e1)
        Nothing -> do
          typecheckExpr0Single trav tyEnv e1
    MandatoryBinder labelOpt (x, tye) : params' -> do
      a0tye <- typecheckTypeExpr0 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv) params' tyeBodyOpt e1
      let ax = AssVarStatic svX
      pure (A0TyArrow labelOpt (Just ax, a0tye) a0tye', A0Lam Nothing (ax, strictify a0tye) a0e')
    OmissibleBinder label (x, tye@(Expr locTye _)) : params' -> do
      a0tye <- typecheckTypeExpr0 trav tyEnv tye
      case a0tye of
        A0TyMaybe a0tyeElem -> do
          svX <- generateFreshVar (Just x)
          (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv) params' tyeBodyOpt e1
          let ax = AssVarStatic svX
          pure (A0TyOmsArrow label (Just ax, a0tyeElem) a0tye', A0Lam Nothing (ax, strictify a0tye) a0e')
        _ -> do
          spanInFile <- askSpanInFile locTye
          typeError trav $ NonMaybeAnnotForLamOms0 spanInFile a0tye
    InferableBinder (x, tye) : params' -> do
      a0tye <- typecheckTypeExpr0 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addVal x (Ass0Entry a0tye (Right svX)) tyEnv) params' tyeBodyOpt e1
      let ax = AssVarStatic svX
      pure (A0TyInfArrow (ax, a0tye) a0tye', A0Lam Nothing (ax, strictify a0tye) a0e')
    TypeBinder tyvar : params' -> do
      atyvar <- generateFreshTypeVar tyvar
      (a0tye', a0e') <- typecheckLetInBody0 trav (TypeEnv.addTypeVar tyvar (TypeVarEntry0 atyvar) tyEnv) params' tyeBodyOpt e1
      pure (A0TyForAll atyvar a0tye', A0LamType atyvar a0e')

forceExpr1 :: trav -> TypeEnv -> Ass1TypeExpr -> Expr -> M trav Ass1Expr
forceExpr1 trav tyEnv a1tyeReq e@(Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  case eMain of
    Literal (LitList es) ->
      case a1tyeReq of
        A1TyList a1tyeElem -> do
          a1es <- mapM (forceExpr1 trav tyEnv a1tyeElem) es
          pure $ A1Literal (ALitList a1es)
        _ ->
          typeError trav $ CannotForceType1 spanInFile a1tyeReq
    Tuple es -> do
      case a1tyeReq of
        A1TyProduct a1tyesReq ->
          case TwoOrMore.zipExact a1tyesReq es of
            Just zipped -> do
              a1es <- mapM (uncurry (forceExpr1 trav tyEnv)) zipped
              pure $ A1Tuple a1es
            Nothing ->
              typeError trav $ CannotForceType1 spanInFile a1tyeReq
        _ -> do
          typeError trav $ CannotForceType1 spanInFile a1tyeReq
    Constructor (mods, ctor) ->
      case (mods, ctor) of
        ([], "Nothing") ->
          case a1tyeReq of
            A1TyMaybe _a1tyeElem -> pure $ A1Constructor "Nothing" []
            _ -> typeError trav $ CannotForceType1 spanInFile a1tyeReq
        _ -> do
          (a1tye, a1e) <- typecheckExpr1Single trav tyEnv e
          (eq, _varSolution, _tyvar1Solution) <- makeEquation1 trav loc Set.empty Set.empty a1tye a1tyeReq
          pure $ applyEquationCast loc eq a1e
    IfThenElse e0 e1 e2 -> do
      (a1tye0, a1e0) <- typecheckExpr1Single trav tyEnv e0
      case a1tye0 of
        A1TyPrim (A1TyPrimBase ATyPrimBool) -> do
          a1e1 <- forceExpr1 trav tyEnv a1tyeReq e1
          a1e2 <- forceExpr1 trav tyEnv a1tyeReq e2
          pure $ A1IfThenElse a1e0 a1e1 a1e2
        _ -> do
          let Expr loc0 _ = e0
          spanInFile0 <- askSpanInFile loc0
          typeError trav $ NotABoolTypeForStage1 spanInFile0 a1tye0
    _ -> do
      (a1tye, a1e) <- typecheckExpr1Single trav tyEnv e
      (eq, _varSolution, _tyvar1Solution) <- makeEquation1 trav loc Set.empty Set.empty a1tye a1tyeReq
      pure $ applyEquationCast loc eq a1e

typecheckExpr1Single :: trav -> TypeEnv -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckExpr1Single trav tyEnv e@(Expr loc _) = do
  (result1, a1e) <- typecheckExpr1 trav tyEnv [] e
  case result1 of
    Pure a1tye ->
      pure (a1tye, a1e)
    _ -> do
      spanInFile <- askSpanInFile loc
      bug $ "non-empty result1: " ++ show spanInFile

typecheckExpr1 :: trav -> TypeEnv -> AppContext -> Expr -> M trav (Result1, Ass1Expr)
typecheckExpr1 trav tyEnv appCtx (Expr loc eMain) = do
  spanInFile <- askSpanInFile loc
  completeImplicit
    <$> case eMain of
      Constructor (mods, ctor) ->
        case (mods, ctor) of
          ([], "Just") ->
            case appCtx of
              [] ->
                typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
              [AppArg1 Nothing a1tye1] -> do
                svX <- generateFreshVar Nothing
                let ax = AssVarStatic svX
                let a1eRet = A1Lam Nothing (ax, a1tye1) (A1Constructor "Just" [A1Var ax])
                pure (Cast1 Nothing a1tye1 (Pure (A1TyMaybe a1tye1)), a1eRet)
              _ ->
                error "TODO (error): other app contexts"
          ([], "Nothing") ->
            typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
          (_, _) ->
            typeError trav $ UnboundConstructor spanInFile mods ctor
      Product e1 rest ->
        typecheckExpr1 trav tyEnv appCtx (convertProductToApp e1 rest)
      Literal lit ->
        case appCtx of
          [] -> do
            (a1tye, alit) <-
              case lit of
                LitInt n ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimInt), ALitInt n)
                LitFloat r ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimFloat), ALitFloat r)
                LitUnit ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimUnit), ALitUnit)
                LitBool b ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimBool), ALitBool b)
                LitString t ->
                  pure (A1TyPrim (A1TyPrimBase ATyPrimString), ALitString t)
                LitList es ->
                  case es of
                    [] ->
                      typeError trav $ CannotSynthesizeTypeFromExpr spanInFile
                    eFirst : esTail -> do
                      (a1tyeFirst, a1eFirst) <- typecheckExpr1Single trav tyEnv eFirst
                      a1esTail <-
                        mapM
                          ( \e@(Expr locElem _) -> do
                              (a1tye, a1e) <- typecheckExpr1Single trav tyEnv e
                              (eq, _varSolution, _tyvar1Solution) <-
                                makeEquation1 trav locElem Set.empty Set.empty a1tye a1tyeFirst
                              pure (applyEquationCast locElem eq a1e)
                          )
                          esTail
                      pure (A1TyList a1tyeFirst, ALitList (a1eFirst : a1esTail))
                LitVec ns -> do
                  let vec = Vector.fromList ns
                  pure (A1TyPrim (a1TyVec (A0Literal (ALitInt (Vector.length vec)))), ALitVec vec)
                LitMat nss -> do
                  mat <-
                    liftEither . mapLeft (\e -> (InvalidMatrixLiteral spanInFile e, trav)) $
                      Matrix.fromRows nss
                  pure (A1TyPrim (uncurry a1TyMat (both (A0Literal . ALitInt) (Matrix.size mat))), ALitMat mat)
            pure (Pure a1tye, A1Literal alit)
          _ : _ ->
            typeError trav $ CannotApplyLiteral spanInFile
      Var (ms, x) -> do
        (a1tye, a1e) <- typecheckValVar1 trav loc tyEnv ms x
        (result, _) <- instantiateGuidedByAppContext1 trav loc Set.empty appCtx a1tye
        pure (result, a1e)
      Lam recOpt labelOpt (x1, tye1) e2 ->
        case appCtx of
          [] -> do
            svX1 <- generateFreshVar (Just x1)
            case recOpt of
              Nothing -> do
                a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
                (a1tye2, a1e2) <- do
                  let tyEnv' = TypeEnv.addVal x1 (Ass1Entry a1tye1 (Right svX1)) tyEnv
                  typecheckExpr1Single trav tyEnv' e2
                let ax1 = AssVarStatic svX1
                pure (Pure (A1TyArrow labelOpt a1tye1 a1tye2), A1Lam Nothing (ax1, a1tye1) a1e2)
              Just (f, tyeRec) -> do
                svF <- generateFreshVar (Just f)
                a1tyeRec <- typecheckTypeExpr1 trav tyEnv tyeRec
                a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
                (a1tye2, a1e2) <- do
                  let tyEnv' =
                        tyEnv
                          & TypeEnv.addVal x1 (Ass1Entry a1tye1 (Right svX1))
                          & TypeEnv.addVal f (Ass1Entry a1tyeRec (Right svF))
                  typecheckExpr1Single trav tyEnv' e2
                let ax1 = AssVarStatic svX1
                let af = AssVarStatic svF
                let a1tyeSynth = A1TyArrow labelOpt a1tye1 a1tye2
                (eq, _varSolution, _tyvar1Solution) <-
                  makeEquation1 trav loc Set.empty Set.empty a1tyeSynth a1tyeRec
                pure (Pure a1tyeRec, applyEquationCast loc eq (A1Lam (Just (af, a1tyeRec)) (ax1, a1tye1) a1e2))
          _ : _ ->
            -- TODO (enhance): consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamWithArguments appCtx
      App e1 labelOpt e2 -> do
        (a1tye2, a1e2) <- typecheckExpr1Single trav tyEnv e2
        (result1, a1e1) <- typecheckExpr1 trav tyEnv (AppArg1 labelOpt a1tye2 : appCtx) e1
        case result1 of
          Cast1 cast _a1tye11 result ->
            -- Embeds type equality assertion at stage 0 here!
            pure (result, A1App a1e1 (applyCast1 cast a1e2))
          _ ->
            bug "stage-1, App, fun, not a Cast1"
      LamOms label (x1, tye1@(Expr locTye1 _)) e2 ->
        case appCtx of
          [] -> do
            svX1 <- generateFreshVar (Just x1)
            a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
            case a1tye1 of
              A1TyMaybe a1tyeElem1 -> do
                (a1tye2, a1e2) <- do
                  let tyEnv' = TypeEnv.addVal x1 (Ass1Entry a1tye1 (Right svX1)) tyEnv
                  typecheckExpr1Single trav tyEnv' e2
                let ax1 = AssVarStatic svX1
                pure (Pure (A1TyOmsArrow label a1tyeElem1 a1tye2), A1Lam Nothing (ax1, a1tye1) a1e2)
              _ -> do
                spanInFile1 <- askSpanInFile locTye1
                typeError trav $ NonMaybeAnnotForLamOms1 spanInFile1 a1tye1
          _ : _ ->
            -- TODO (enhance): consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamOmsWithArguments appCtx
      AppOms e1 label e2 -> do
        (a1tye2, a1e2) <- typecheckExpr1Single trav tyEnv e2
        (result1, a1e1) <- typecheckExpr1 trav tyEnv (AppArgOmsGiven1 label a1tye2 : appCtx) e1
        case result1 of
          CastOmsGiven1 cast _a1tye11 result ->
            -- Embeds type equality assertion at stage 0 here!
            pure (result, A1App a1e1 (A1Constructor "Just" [applyCast1 cast a1e2]))
          _ ->
            bug "stage-1, AppOms, fun, not a CastOms1"
      LamInf _ _ ->
        typeError trav $ CannotUseLamInfAtStage1 spanInFile
      AppInfGiven _ _ ->
        typeError trav $ CannotUseAppInfGivenAtStage1 spanInFile
      AppInfOmitted _ ->
        typeError trav $ CannotUseAppInfOmittedAtStage1 spanInFile
      LetIn x params tyeBodyOpt eBody e2 -> do
        svX <- generateFreshVar (Just x)
        (a1tye1, a1e1) <- typecheckLetInBody1 trav tyEnv params tyeBodyOpt eBody
        (result2, a1e2) <-
          typecheckExpr1 trav (TypeEnv.addVal x (Ass1Entry a1tye1 (Right svX)) tyEnv) appCtx e2
        let ax = AssVarStatic svX
        if ax `occurs1` result2
          then typeError trav $ VarOccursFreelyInAss1Type spanInFile x result2
          else pure (result2, A1LetIn (ax, a1tye1) a1e1 a1e2)
      LetRecIn f params tyeBody eBody e2 -> do
        a1tye1Rec <- constructFunTypeExpr1 trav loc tyEnv params tyeBody
        (labelOpt, x0, tyeParam0, paramsRest) <-
          case params of
            MandatoryBinder labelOpt' (x0', tyeParam0') : paramsRest' ->
              pure (labelOpt', x0', tyeParam0', paramsRest')
            OmissibleBinder {} : _ ->
              typeError trav $ LetRecParamsCannotStartWithImplicit spanInFile
            InferableBinder {} : _ ->
              typeError trav $ LetRecParamsCannotStartWithImplicit spanInFile
            TypeBinder {} : _ ->
              error "TODO: typecheckExpr1, LetRecIn, TypeBinder"
            [] ->
              typeError trav $ LetRecRequiresNonEmptyParams spanInFile
        svFInner <- generateFreshVar (Just f)
        let afInner = AssVarStatic svFInner
        svX0 <- generateFreshVar (Just x0)
        let ax0 = AssVarStatic svX0
        a1tyeParam0 <- typecheckTypeExpr1 trav tyEnv tyeParam0
        (a1tyeRestSynth, a1eRest) <- do
          let tyEnv' =
                tyEnv
                  & TypeEnv.addVal f (Ass1Entry a1tye1Rec (Right svFInner))
                  & TypeEnv.addVal x0 (Ass1Entry a1tyeParam0 (Right svX0))
          typecheckLetInBody1 trav tyEnv' paramsRest (Just tyeBody) eBody
        let a1tye1Synth = A1TyArrow labelOpt a1tyeParam0 a1tyeRestSynth
        (eq, _varSolution, _tyvar1Solution) <- makeEquation1 trav loc Set.empty Set.empty a1tye1Synth a1tye1Rec
        let a1e1 = applyEquationCast loc eq (A1Lam (Just (afInner, a1tye1Rec)) (ax0, a1tyeParam0) a1eRest)
        svFOuter <- generateFreshVar (Just f)
        let afOuter = AssVarStatic svFOuter
        (result2, a1e2) <- do
          let tyEnv' = TypeEnv.addVal f (Ass1Entry a1tye1Rec (Right svFOuter)) tyEnv
          typecheckExpr1 trav tyEnv' appCtx e2
        if afOuter `occurs1` result2
          then typeError trav $ VarOccursFreelyInAss1Type spanInFile f result2
          else pure (result2, A1LetIn (afOuter, a1tye1Rec) a1e1 a1e2)
      LetTupleIn xs e1@(Expr loc1 _) e2 -> do
        (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
        case a1tye1 of
          A1TyProduct a1tyes -> do
            zipped <-
              case TwoOrMore.zipExact xs a1tyes of
                Just zipped' ->
                  pure zipped'
                Nothing -> do
                  spanInFile1 <- askSpanInFile loc1
                  typeError trav $ LetTupleLengthMismatch1 spanInFile1 xs a1tyes
            triples <-
              mapM
                ( \(x, a1tye) -> do
                    svX <- generateFreshVar (Just x)
                    pure ((x, a1tye), svX)
                )
                zipped
            (result2, a1e2) <- do
              let tyEnv2 =
                    foldl'
                      ( \tyEnv' ((x, a1tye), svX) ->
                          TypeEnv.addVal x (Ass1Entry a1tye (Right svX)) tyEnv'
                      )
                      tyEnv
                      triples
              typecheckExpr1 trav tyEnv2 appCtx e2
            pure (result2, A1LetTupleIn (fmap (AssVarStatic . snd) triples) a1e1 a1e2)
          _ -> do
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotATupleAtStage1 spanInFile1 a1tye1
      LetOpenIn m e -> do
        case TypeEnv.findModule m tyEnv of
          Nothing ->
            typeError trav $ UnboundModule spanInFile m
          Just (ModuleEntry sigr) -> do
            let tyEnv' = TypeEnv.appendSigRecord tyEnv sigr
            typecheckExpr1 trav tyEnv' appCtx e
      Sequential e1 e2 -> do
        (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
        case a1tye1 of
          A1TyPrim (A1TyPrimBase ATyPrimUnit) -> do
            (result2, a1e2) <- typecheckExpr1 trav tyEnv appCtx e2
            pure (result2, A1Sequential a1e1 a1e2)
          _ -> do
            let Expr loc1 _ = e1
            spanInFile1 <- askSpanInFile loc1
            typeError trav $ NotAUnitTypeForStage1 spanInFile1 a1tye1
      Tuple es -> do
        case appCtx of
          [] -> do
            pairs <- mapM (typecheckExpr1Single trav tyEnv) es
            pure (Pure (A1TyProduct (fmap fst pairs)), A1Tuple (fmap snd pairs))
          _ : _ ->
            typeError trav $ CannotApplyTuple spanInFile
      IfThenElse e0 e1 e2 -> do
        (a1tye0, a1e0) <- typecheckExpr1Single trav tyEnv e0
        case a1tye0 of
          A1TyPrim (A1TyPrimBase ATyPrimBool) ->
            case appCtx of
              [] -> do
                (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
                (a1tye2, a1e2) <- typecheckExpr1Single trav tyEnv e2
                (eq, _varSolution, _tyvar1Solution) <- makeEquation1 trav loc Set.empty Set.empty a1tye2 a1tye1
                pure (Pure a1tye1, A1IfThenElse a1e0 a1e1 (applyEquationCast loc eq a1e2))
              _ : _ -> do
                typeError trav $ Stage1IfThenElseRestrictedToEmptyContext spanInFile appCtx
          _ -> do
            let Expr loc0 _ = e0
            spanInFile0 <- askSpanInFile loc0
            typeError trav $ NotABoolTypeForStage1 spanInFile0 a1tye0
      Case e0 branches -> do
        (a1tye0, _a1e0) <- typecheckExpr1Single trav tyEnv e0
        case appCtx of
          [] -> do
            _triples <- mapM (forceBranch1 trav tyEnv a1tye0) branches
            error "TODO: typecheckExpr1, Case"
          _ : _ -> do
            typeError trav $ Stage1CaseRestrictedToEmptyContext spanInFile appCtx
      As e1 tye2 ->
        case appCtx of
          [] -> do
            a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
            a1e1 <- forceExpr1 trav tyEnv a1tye2 e1
            pure (Pure a1tye2, a1e1)
          _ : _ ->
            typeError trav $ Unsupported spanInFile $ AsWithArguments appCtx
      Bracket _ ->
        typeError trav $ CannotUseBracketAtStage1 spanInFile
      Escape e1 -> do
        (result1, a0e1) <- typecheckExpr0 trav tyEnv appCtx e1
        result <-
          mapMPure
            ( \case
                A0TyCode a1tye1 ->
                  pure a1tye1
                a0tye1 -> do
                  let Expr loc1 _ = e1
                  spanInFile1 <- askSpanInFile loc1
                  typeError trav $ NotACodeType spanInFile1 a0tye1
            )
            result1
        pure (result, A1Escape a0e1)
      LamInfType tyvar1 e2 ->
        case appCtx of
          [] -> do
            atyvar1 <- generateFreshTypeVar tyvar1
            (a1tye2, a1e2) <- do
              let tyEnv' = TypeEnv.addTypeVar tyvar1 (TypeVarEntry1 atyvar1) tyEnv
              typecheckExpr1Single trav tyEnv' e2
            pure (Pure (A1TyForAll atyvar1 a1tye2), A1LamType atyvar1 a1e2)
          _ : _ ->
            error "TODO: typecheckExpr0, LamInfType, non-empty context"
      AppInfType e1 tye2 -> do
        a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
        (result1, a1e1) <- typecheckExpr1 trav tyEnv (AppArgInfTypeGiven1 a1tye2 : appCtx) e1
        case result1 of
          Instantiated1 result -> do
            pure (result, A1AppType a1e1 a1tye2)
          _ -> do
            bug "stage-1, AppInfType"
      Persistent _ ->
        typeError trav $ CannotUsePersistent spanInFile
      (TyVar {}; TyArrow {}; TyOmsArrow {}; TyInfArrow {}; TyRefinement {}; TyForAll {}) ->
        typeError trav $ InvalidSyntaxAsExpr spanInFile
  where
    completeImplicit pair@(result, a1e) =
      case result of
        InsertOmitted1 result' ->
          completeImplicit (result', A1App a1e (A1Constructor "Nothing" []))
        InsertInferredType1 a1tyeInferred result' ->
          completeImplicit (result', A1AppType a1e a1tyeInferred)
        _ ->
          pair

typecheckLetInBody1 :: trav -> TypeEnv -> [LamBinder] -> Maybe TypeExpr -> Expr -> M trav (Ass1TypeExpr, Ass1Expr)
typecheckLetInBody1 trav tyEnv params tyeBodyOpt e1 =
  case params of
    [] -> do
      case tyeBodyOpt of
        Just tyeBody -> do
          a1tye1req <- typecheckTypeExpr1 trav tyEnv tyeBody
          a1e1 <- forceExpr1 trav tyEnv a1tye1req e1
          pure (a1tye1req, a1e1)
        Nothing -> do
          typecheckExpr1Single trav tyEnv e1
    MandatoryBinder labelOpt (x, tye) : params' -> do
      a1tye <- typecheckTypeExpr1 trav tyEnv tye
      svX <- generateFreshVar (Just x)
      (a1tye', a1e') <- typecheckLetInBody1 trav (TypeEnv.addVal x (Ass1Entry a1tye (Right svX)) tyEnv) params' tyeBodyOpt e1
      let ax = AssVarStatic svX
      pure (A1TyArrow labelOpt a1tye a1tye', A1Lam Nothing (ax, a1tye) a1e')
    OmissibleBinder label (x, tye@(Expr locTye _)) : params' -> do
      a1tye <- typecheckTypeExpr1 trav tyEnv tye
      case a1tye of
        A1TyMaybe a1tyeElem -> do
          svX <- generateFreshVar (Just x)
          (a1tye', a1e') <- typecheckLetInBody1 trav (TypeEnv.addVal x (Ass1Entry a1tye (Right svX)) tyEnv) params' tyeBodyOpt e1
          let ax = AssVarStatic svX
          pure (A1TyOmsArrow label a1tyeElem a1tye', A1Lam Nothing (ax, a1tye) a1e')
        _ -> do
          spanInFile1 <- askSpanInFile locTye
          typeError trav $ NonMaybeAnnotForLamOms1 spanInFile1 a1tye
    InferableBinder (_x, tye) : _params' -> do
      let Expr loc _ = tye -- TODO (enhance): give a better code position
      spanInFile <- askSpanInFile loc
      typeError trav $ CannotUseLamInfAtStage1 spanInFile
    TypeBinder tyvar : params' -> do
      atyvar <- generateFreshTypeVar tyvar
      (a1tye', a1e') <- typecheckLetInBody1 trav (TypeEnv.addTypeVar tyvar (TypeVarEntry1 atyvar) tyEnv) params' tyeBodyOpt e1
      pure (A1TyForAll atyvar a1tye', A1LamType atyvar a1e')

mapMPure :: (af StaticVar -> M trav (bf StaticVar)) -> ResultF af StaticVar -> M trav (ResultF bf StaticVar)
mapMPure f = go
  where
    go (Pure v) = Pure <$> f v
    go (Cast0 cast a0tye r) = Cast0 cast a0tye <$> go r
    go (Cast1 eq a1tye r) = Cast1 eq a1tye <$> go r
    go (CastOmsGiven0 cast a0tye r) = CastOmsGiven0 cast a0tye <$> go r
    go (InsertOmitted0 r) = InsertOmitted0 <$> go r
    go (CastOmsGiven1 eq a1tye r) = CastOmsGiven1 eq a1tye <$> go r
    go (InsertOmitted1 r) = InsertOmitted1 <$> go r
    go (CastInfGiven0 a0e a0tye r) = CastInfGiven0 a0e a0tye <$> go r
    go (FillInferred0 a0e r) = FillInferred0 a0e <$> go r
    go (InsertInferred0 a0e r) = InsertInferred0 a0e <$> go r
    go (Instantiated0 r) = Instantiated0 <$> go r
    go (InsertInferredType0 a0tye r) = InsertInferredType0 a0tye <$> go r
    go (Instantiated1 r) = Instantiated1 <$> go r
    go (InsertInferredType1 a1tye r) = InsertInferredType1 a1tye <$> go r

validateIntLiteral :: trav -> Span -> Ass0Expr -> M trav Int
validateIntLiteral trav loc a0e =
  case a0e of
    A0Literal (ALitInt n) ->
      pure n
    _ -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ NotAnIntLitArgAtStage0 spanInFile a0e

validateIntListLiteral :: trav -> Span -> Ass0Expr -> M trav [Int]
validateIntListLiteral trav loc a0e =
  case a0e of
    A0Literal (ALitList a0es) -> do
      spanInFile <- askSpanInFile loc
      mapM
        ( \case
            A0Literal (ALitInt n) -> pure n
            _ -> typeError trav $ NotAnIntListLitArgAtStage0 spanInFile a0e
        )
        a0es
    _ -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ NotAnIntListLitArgAtStage0 spanInFile a0e

typecheckTypeExpr0 :: trav -> TypeEnv -> TypeExpr -> M trav Ass0TypeExpr
typecheckTypeExpr0 trav tyEnv (Expr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    Constructor (mods, tyName) ->
      case mods of
        [] ->
          case tyName of
            "Nat" ->
              pure BuiltIn.tyNat
            _ ->
              case validatePrimBaseType tyName of
                Just tyPrimBase -> pure $ A0TyPrim (A0TyPrimBase tyPrimBase) Nothing
                Nothing -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile mods tyName 0
        _ : _ ->
          typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile mods tyName 0
    App _ labelOpt _ -> do
      () <-
        case labelOpt of
          Nothing -> pure ()
          Just _ -> typeError trav $ InvalidSyntaxAsTypeExpr spanInFile
      ((mods, tyName), args) <- collectTypeArgs trav loc tyeMain
      case (tyName, args) of
        ("List", [arg1]) -> do
          a0tye1 <- typecheckTypeExpr0 trav tyEnv arg1
          pure $ A0TyList a0tye1 Nothing
        ("Maybe", [arg1]) -> do
          a0tye1 <- typecheckTypeExpr0 trav tyEnv arg1
          pure $ A0TyMaybe a0tye1
        ("Vec", [arg1@(Expr loc1 _)]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg1
          n1 <- validateIntLiteral trav loc1 a0e1
          pure $ A0TyPrim (a0TyVec n1) Nothing
        ("Mat", [arg1@(Expr loc1 _), arg2@(Expr loc2 _)]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg2
          n1 <- validateIntLiteral trav loc1 a0e1
          n2 <- validateIntLiteral trav loc2 a0e2
          pure $ A0TyPrim (a0TyMat n1 n2) Nothing
        ("Tensor", [arg@(Expr loc' _)]) -> do
          a0e <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) arg
          ns <- validateIntListLiteral trav loc' a0e
          pure $ A0TyPrim (A0TyTensor ns) Nothing
        ("Dataset", [arg1@(Expr loc1 _), arg2@(Expr loc2 _), arg3@(Expr loc3 _), arg4@(Expr loc4 _)]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg2
          a0e3 <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) arg3
          a0e4 <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) arg4
          numTrain <- validateIntLiteral trav loc1 a0e1
          numTest <- validateIntLiteral trav loc2 a0e2
          image <- validateIntListLiteral trav loc3 a0e3
          label <- validateIntListLiteral trav loc4 a0e4
          let datasetParam = DatasetParam {numTrain, numTest, image, label}
          pure $ A0TyPrim (A0TyDataset datasetParam) Nothing
        ("Lstm", [arg1@(Expr loc1 _), arg2@(Expr loc2 _)]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg2
          inputSize <- validateIntLiteral trav loc1 a0e1
          hiddenSize <- validateIntLiteral trav loc2 a0e2
          pure $ A0TyPrim (A0TyLstm inputSize hiddenSize) Nothing
        ("TextHelper", [arg1@(Expr loc1 _)]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg1
          labels <- validateIntLiteral trav loc1 a0e1
          pure $ A0TyPrim (A0TyTextHelper labels) Nothing
        _ ->
          typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile mods tyName (length args)
    TyVar tyvar -> do
      tyvarEntry <- findTypeVar trav loc tyvar tyEnv
      case tyvarEntry of
        TypeVarEntry0 atyvar -> pure $ A0TyVar atyvar
        TypeVarEntry1 _ -> typeError trav $ NotAStage0TypeVar spanInFile tyvar
    TyArrow labelOpt (xOpt, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      (tyEnv', svXOpt) <-
        case xOpt of
          Just x -> do
            svX <- generateFreshVar (Just x)
            pure (TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv, Just svX)
          Nothing ->
            pure (tyEnv, Nothing)
      a0tye2 <- typecheckTypeExpr0 trav tyEnv' tye2
      let axOpt = AssVarStatic <$> svXOpt
      pure $ A0TyArrow labelOpt (axOpt, a0tye1) a0tye2
    Bracket tye1 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      pure $ A0TyCode a1tye1
    TyOmsArrow label (xOpt, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      (tyEnv', svXOpt) <-
        case xOpt of
          Just x -> do
            svX <- generateFreshVar (Just x)
            pure (TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv, Just svX)
          Nothing ->
            pure (tyEnv, Nothing)
      a0tye2 <- typecheckTypeExpr0 trav tyEnv' tye2
      let axOpt = AssVarStatic <$> svXOpt
      pure $ A0TyOmsArrow label (axOpt, a0tye1) a0tye2
    TyInfArrow (x, tye1) tye2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      svX <- generateFreshVar (Just x)
      a0tye2 <- do
        let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
        typecheckTypeExpr0 trav tyEnv' tye2
      let ax = AssVarStatic svX
      pure $ A0TyInfArrow (ax, a0tye1) a0tye2
    TyRefinement x tye1 e2 -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      svX <- generateFreshVar (Just x)
      (a0tye2, a0e2) <- do
        let tyEnv' = TypeEnv.addVal x (Ass0Entry a0tye1 (Right svX)) tyEnv
        typecheckExpr0Single trav tyEnv' e2
      case a0tye2 of
        A0TyPrim (A0TyPrimBase ATyPrimBool) _maybePredForBool -> do
          let ax = AssVarStatic svX
          case a0tye1 of
            A0TyPrim a0tyPrim Nothing -> do
              pure $
                A0TyPrim a0tyPrim . Just $
                  A0Lam Nothing (ax, strictify a0tye1) a0e2
            A0TyPrim a0tyPrim (Just a0ePredForBase) -> do
              pure $
                A0TyPrim a0tyPrim . Just $
                  A0Lam Nothing (ax, strictify a0tye1) $
                    A0App (A0App ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
            A0TyList a0tyeElem Nothing -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) a0e2
            A0TyList a0tyeElem (Just a0ePredForBase) -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) $
                    A0App (A0App ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
            _ -> do
              let Expr loc1 _ = tye1
              spanInFile1 <- askSpanInFile loc1
              typeError trav $ InvalidTypeForRefinement spanInFile1 a0tye1
        _ -> do
          let Expr loc2 _ = e2
          spanInFile2 <- askSpanInFile loc2
          typeError trav $ NotABoolTypeForStage0 spanInFile2 a0tye2
    Product tye1 rest -> do
      a0tye1 <- typecheckTypeExpr0 trav tyEnv tye1
      a0tyesRest <-
        mapM
          ( \((_locOp, op), tye) ->
              case op of
                "*" -> typecheckTypeExpr0 trav tyEnv tye
                _ -> typeError trav $ InvalidSyntaxAsTypeExpr spanInFile
          )
          rest
      pure $ A0TyProduct (TwoOrMore.make1 a0tye1 a0tyesRest)
    TyForAll tyvar tye1 -> do
      atyvar <- generateFreshTypeVar tyvar
      a0tye1 <- do
        let tyEnv' = TypeEnv.addTypeVar tyvar (TypeVarEntry0 atyvar) tyEnv
        typecheckTypeExpr0 trav tyEnv' tye1
      pure $ A0TyForAll atyvar a0tye1
    (Literal {}; Var {}; Lam {}; LetIn {}; LetRecIn {}; LetTupleIn {}; IfThenElse {}; Case {}; As {}; Escape _; LamOms {}; AppOms {}; LamInf {}; AppInfGiven {}; AppInfOmitted {}; LetOpenIn {}; Sequential {}; Tuple {}; LamInfType {}; AppInfType {}; Persistent {}) ->
      typeError trav $ InvalidSyntaxAsTypeExpr spanInFile

ass0exprAnd :: Ass0Expr
ass0exprAnd = A0BuiltInName (BuiltInArity2 BIAnd)

validatePersistentExprArg1 :: trav -> Expr -> M trav Expr
validatePersistentExprArg1 trav (Expr loc eMain) =
  case eMain of
    Persistent e ->
      pure e
    _ -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ CannotUseNormalArgAtStage1 spanInFile

collectTypeArgs :: trav -> Span -> TypeExprMain -> M trav (([ModuleName], TypeName), [Expr])
collectTypeArgs trav loc = go
  where
    go = \case
      App (Expr _ eFunMain) Nothing eArg -> do
        (qualTyName, eArgs) <- go eFunMain
        pure (qualTyName, eArgs ++ [eArg])
      Constructor (mods, tyName) -> do
        pure ((mods, tyName), [])
      _ -> do
        spanInFile <- askSpanInFile loc
        typeError trav $ InvalidSyntaxAsTypeExpr spanInFile

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav Ass1TypeExpr
typecheckTypeExpr1 trav tyEnv (Expr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    Constructor (mods, tyName) ->
      case mods of
        [] ->
          case validatePrimBaseType tyName of
            Just tyPrimBase -> pure $ A1TyPrim (A1TyPrimBase tyPrimBase)
            Nothing -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile mods tyName 0
        _ : _ ->
          typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile mods tyName 0
    App _ labelOpt _ -> do
      () <-
        case labelOpt of
          Nothing -> pure ()
          Just _ -> typeError trav $ InvalidSyntaxAsTypeExpr spanInFile
      ((mods, tyName), args) <- collectTypeArgs trav loc tyeMain
      case (tyName, args) of
        ("List", [tye]) -> do
          a1tye <- typecheckTypeExpr1 trav tyEnv tye
          pure $ A1TyList a1tye
        ("Maybe", [tye]) -> do
          a1tye <- typecheckTypeExpr1 trav tyEnv tye
          pure $ A1TyMaybe a1tye
        ("Vec", [arg]) -> do
          e <- validatePersistentExprArg1 trav arg
          a0e <- forceExpr0 trav tyEnv BuiltIn.tyNat e
          pure $ A1TyPrim (a1TyVec a0e)
        ("Mat", [arg1, arg2]) -> do
          e1 <- validatePersistentExprArg1 trav arg1
          e2 <- validatePersistentExprArg1 trav arg2
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat e2
          pure $ A1TyPrim (a1TyMat a0e1 a0e2)
        ("Tensor", [arg]) -> do
          logShapeAnnot (ShapeAnnotLog loc)
          e <- validatePersistentExprArg1 trav arg
          a0eList <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) e
          pure $ A1TyPrim (A1TyTensor a0eList)
        ("Dataset", [arg1, arg2, arg3, arg4]) -> do
          logShapeAnnot (ShapeAnnotLog loc)
          e1 <- validatePersistentExprArg1 trav arg1
          e2 <- validatePersistentExprArg1 trav arg2
          e3 <- validatePersistentExprArg1 trav arg3
          e4 <- validatePersistentExprArg1 trav arg4
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat e2
          a0e3 <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) e3
          a0e4 <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) e4
          let datasetParam =
                DatasetParam
                  { numTrain = a0e1,
                    numTest = a0e2,
                    image = Identity a0e3,
                    label = Identity a0e4
                  }
          pure $ A1TyPrim (A1TyDataset datasetParam)
        ("Lstm", [arg1, arg2]) -> do
          logShapeAnnot (ShapeAnnotLog loc)
          e1 <- validatePersistentExprArg1 trav arg1
          e2 <- validatePersistentExprArg1 trav arg2
          a0eInputSize <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          a0eHiddenSize <- forceExpr0 trav tyEnv BuiltIn.tyNat e2
          pure $ A1TyPrim (A1TyLstm a0eInputSize a0eHiddenSize)
        ("TextHelper", [arg1]) -> do
          logShapeAnnot (ShapeAnnotLog loc)
          e1 <- validatePersistentExprArg1 trav arg1
          a0eLabels <- forceExpr0 trav tyEnv BuiltIn.tyNat e1
          pure $ A1TyPrim (A1TyTextHelper a0eLabels)
        _ ->
          typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile mods tyName (length args)
    TyVar tyvar -> do
      tyvarEntry <- findTypeVar trav loc tyvar tyEnv
      case tyvarEntry of
        TypeVarEntry0 _ -> typeError trav $ NotAStage1TypeVar spanInFile tyvar
        TypeVarEntry1 atyvar -> pure $ A1TyVar atyvar
    TyArrow labelOpt (xOpt, tye1) tye2 -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      case xOpt of
        Just x ->
          typeError trav $ FunctionTypeCannotBeDependentAtStage1 spanInFile x
        Nothing -> do
          a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
          pure $ A1TyArrow labelOpt a1tye1 a1tye2
    TyOmsArrow label (xOpt, tye1) tye2 -> do
      case xOpt of
        Just x ->
          typeError trav $ FunctionTypeCannotBeDependentAtStage1 spanInFile x
        Nothing -> do
          a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
          a1tye2 <- typecheckTypeExpr1 trav tyEnv tye2
          pure $ A1TyOmsArrow label a1tye1 a1tye2
    TyInfArrow {} ->
      typeError trav $ CannotUseInfArrowTypeAtStage1 spanInFile
    Bracket {} -> do
      typeError trav $ CannotUseCodeTypeAtStage1 spanInFile
    TyRefinement {} -> do
      typeError trav $ CannotUseRefinementTypeAtStage1 spanInFile
    Product tye1 rest -> do
      a1tye1 <- typecheckTypeExpr1 trav tyEnv tye1
      a1tyesRest <-
        mapM
          ( \((_locOp, op), tye) ->
              case op of
                "*" -> typecheckTypeExpr1 trav tyEnv tye
                _ -> typeError trav $ InvalidSyntaxAsTypeExpr spanInFile
          )
          rest
      pure $ A1TyProduct (TwoOrMore.make1 a1tye1 a1tyesRest)
    TyForAll tyvar tye1 -> do
      atyvar <- generateFreshTypeVar tyvar
      a1tye1 <- do
        let tyEnv' = TypeEnv.addTypeVar tyvar (TypeVarEntry1 atyvar) tyEnv
        typecheckTypeExpr1 trav tyEnv' tye1
      pure $ A1TyForAll atyvar a1tye1
    (Literal _; Var _; Lam {}; LetIn {}; LetRecIn {}; LetTupleIn {}; IfThenElse {}; Case {}; As {}; Escape _; LamOms {}; AppOms {}; LamInf {}; AppInfGiven {}; AppInfOmitted {}; LetOpenIn {}; Sequential {}; Tuple {}; LamInfType {}; AppInfType {}; Persistent {}) ->
      typeError trav $ InvalidSyntaxAsTypeExpr spanInFile

validatePersistentType :: trav -> Span -> Ass0TypeExpr -> M trav AssPersTypeExpr
validatePersistentType trav loc a0tye =
  case go a0tye of
    Right aPtye ->
      pure aPtye
    Left unsupportedOpt -> do
      spanInFile <- askSpanInFile loc
      typeError trav $
        case unsupportedOpt of
          Nothing -> InvalidPersistentType spanInFile a0tye
          Just u -> Unsupported spanInFile u
  where
    go = \case
      A0TyPrim a0tyPrim maybePred ->
        case maybePred of
          Nothing -> pure $ APersTyPrim a0tyPrim
          Just _ -> Left Nothing
      A0TyVar atyvar ->
        pure $ APersTyVar atyvar
      A0TyList a0tye' maybePred ->
        case maybePred of
          Nothing -> APersTyList <$> go a0tye'
          Just _ -> Left Nothing
      A0TyMaybe a0tye' ->
        APersTyMaybe <$> go a0tye'
      A0TyProduct a0tyes ->
        APersTyProduct <$> mapM go a0tyes
      A0TyArrow labelOpt (Nothing, a0tye1) a0tye2 -> do
        aPtye1 <- go a0tye1
        aPtye2 <- go a0tye2
        pure $ APersTyArrow labelOpt aPtye1 aPtye2
      A0TyArrow _labelOpt (Just _x, _a0tye1) _a0tye2 -> do
        Left Nothing
      A0TyInfArrow (_x, _a0tye1) _a0tye2 -> do
        Left Nothing
      A0TyOmsArrow _label (Nothing, _a0tye1) _a0tye2 -> do
        Left (Just PersistentFunWithOms)
      A0TyOmsArrow _label (Just _, _a0tye1) _a0tye2 -> do
        Left Nothing
      A0TyCode _ ->
        Left Nothing
      A0TyForAll atyvar a0tye' -> do
        aPtye' <- go a0tye'
        pure $ APersTyForAll atyvar aPtye'

extractFromExternal :: ExternalField -> External -> Maybe Text
extractFromExternal field0 =
  firstJust (\(field, s) -> if field == field0 then Just s else Nothing)

typecheckBind :: trav -> TypeEnv -> Bind -> M trav (SigRecord, [AssBind])
typecheckBind trav tyEnv (Bind loc bindMain) =
  case bindMain of
    BindVal stage x (BindValExternal tye ext) -> do
      extName <-
        case extractFromExternal "builtin" ext of
          Just s ->
            pure s
          Nothing -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ NoBuiltInNameInExternal spanInFile
      let surfaceName = extractFromExternal "surface" ext
      case stage of
        Stage0 -> do
          a0tye <- typecheckTypeExpr0 trav tyEnv tye
          ass0builtInName <-
            case validateExternalName0 extName of
              Just a0builtInName' ->
                pure a0builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          let a0metadata = Ass0Metadata {ass0builtInName, ass0surfaceName = surfaceName}
          pure (SigRecord.singletonVal x (Ass0Entry a0tye (Left a0metadata)), [])
        Stage1 -> do
          ass1builtInName <-
            case validateExternalName1 extName of
              Just ass1builtInName' ->
                pure ass1builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          a1tye <- typecheckTypeExpr1 trav tyEnv tye
          let a1metadata = Ass1Metadata {ass1builtInName, ass1surfaceName = surfaceName}
          pure (SigRecord.singletonVal x (Ass1Entry a1tye (Left a1metadata)), [])
        StagePers -> do
          a0tye <- typecheckTypeExpr0 trav tyEnv tye
          aPtye <- validatePersistentType trav loc a0tye
          assPbuiltInName <-
            case validateExternalName1 extName of
              Just a1builtInName' ->
                pure a1builtInName'
              Nothing -> do
                spanInFile <- askSpanInFile loc
                typeError trav $ UnknownExternalName spanInFile extName
          let aPmetadata = AssPersMetadata {assPbuiltInName, assPsurfaceName = surfaceName}
          pure (SigRecord.singletonVal x (AssPersEntry aPtye aPmetadata), [])
    BindVal stage x (BindValNormal params tyeBodyOpt e) -> do
      svX <- generateFreshVar (Just x)
      let ax = AssVarStatic svX
      case stage of
        Stage0 -> do
          (a0tye, a0e) <- typecheckLetInBody0 trav tyEnv params tyeBodyOpt e
          let sa0tye = strictify a0tye
          pure (SigRecord.singletonVal x (Ass0Entry a0tye (Right svX)), [ABind0 (ax, sa0tye) a0e])
        Stage1 -> do
          (a1tye, a1e) <- typecheckLetInBody1 trav tyEnv params tyeBodyOpt e
          pure (SigRecord.singletonVal x (Ass1Entry a1tye (Right svX)), [ABind1 (ax, a1tye) a1e])
        StagePers -> do
          -- TODO: bind persistent values
          spanInFile <- askSpanInFile loc
          typeError trav $ Unsupported spanInFile (CannotBindPersistentValue x)
    BindModule m binds -> do
      (_, sigr, abinds) <- typecheckBinds trav tyEnv binds
      pure (SigRecord.singletonModule m (ModuleEntry sigr), abinds)

typecheckBinds :: trav -> TypeEnv -> [Bind] -> M trav (TypeEnv, SigRecord, [AssBind])
typecheckBinds trav tyEnv =
  foldM
    ( \(tyEnv', sigr', abinds') bind@(Bind loc _) -> do
        (sigr, abinds) <- typecheckBind trav tyEnv' bind
        case SigRecord.intersection sigr' sigr of
          ([], []) ->
            pure (TypeEnv.appendSigRecord tyEnv' sigr, SigRecord.union sigr' sigr, abinds' ++ abinds)
          (x : _, _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile x
          (_, m : _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile m
    )
    (tyEnv, SigRecord.empty, [])
