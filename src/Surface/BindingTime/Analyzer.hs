module Surface.BindingTime.Analyzer
  ( AnalysisError (..),
    run,
  )
where

import Common.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Common.TokenUtil
import Control.Monad
import Control.Monad.Elaborator hiding (run)
import Control.Monad.Elaborator qualified as Elaborator
import Data.Either.Extra (mapLeft)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Void (absurd)
import Safe.Exact (zipExactMay)
import Staged.Core (Label)
import Staged.Syntax qualified as Staged
import Surface.BindingTime.AnalysisError
import Surface.BindingTime.Constraint
import Surface.BindingTime.Core
import Surface.BindingTime.Env (BindingTimeEnv, BindingTimeModuleEntry (..), BindingTimeValueEntry (..))
import Surface.BindingTime.Env qualified as Env
import Surface.Syntax
import Prelude hiding (succ)

data AnalysisState = AnalysisState
  { nextBindingTimeVarIndex :: Int,
    nextBITypeVarIndex :: Int,
    solution :: Map BITypeVar BITypeMain
  }

newtype AnalysisConfig = AnalysisConfig
  { sourceSpec :: SourceSpec
  }

type M trav a = Elaborator AnalysisState AnalysisConfig AnalysisError trav a

initialState :: AnalysisState
initialState =
  AnalysisState
    { nextBindingTimeVarIndex = 0,
      nextBITypeVarIndex = 0,
      solution = Map.empty
    }

freshBindingTimeVar :: M trav BindingTimeVar
freshBindingTimeVar = do
  st@AnalysisState {nextBindingTimeVarIndex = i} <- getState
  putState $ st {nextBindingTimeVarIndex = i + 1}
  pure $ BindingTimeVar i

freshBITypeVar :: M trav BITypeVar
freshBITypeVar = do
  st@AnalysisState {nextBITypeVarIndex = j} <- getState
  putState $ st {nextBITypeVarIndex = j + 1}
  pure $ BITypeVar j

reconstructBodyType :: [LamBinder] -> BIType -> BIType
reconstructBodyType params bity =
  case (params, bity) of
    ([], _) ->
      bity
    (MandatoryBinder _ _ : params', BIType _ (BITyArrow _ bity')) ->
      reconstructBodyType params' bity'
    (InferableBinder _ : params', BIType _ (BITyInfArrow _ bity')) ->
      reconstructBodyType params' bity'
    (_, _) ->
      error "Bug: reconstructBodyType"

makeLam :: [LamBinder] -> Maybe TypeExpr -> Expr -> Expr
makeLam params tyeBodyOpt eBody = do
  foldr go eBody' params
  where
    -- TODO (enhance): give better range:
    eBody' =
      case tyeBodyOpt of
        Just tyeBody@(Expr ann _) -> Expr ann (As eBody tyeBody)
        Nothing -> eBody

    go :: LamBinder -> Expr -> Expr
    go (MandatoryBinder labelOpt (x, ty@(Expr loc1 _))) e@(Expr loc2 _) =
      -- TODO (enhance): give better range:
      Expr (mergeSpan loc1 loc2) (Lam Nothing labelOpt (x, ty) e)
    go (OmissibleBinder label (x, ty@(Expr loc1 _))) e@(Expr loc2 _) =
      -- TODO (enhance): give better range:
      Expr (mergeSpan loc1 loc2) (LamOms label (x, ty) e)
    go (InferableBinder (x, ty@(Expr loc1 _))) e@(Expr loc2 _) =
      -- TODO (enhance): give better range:
      Expr (mergeSpan loc1 loc2) (LamInf (x, ty) e)

makeRecLam :: trav -> Span -> Var -> [LamBinder] -> TypeExpr -> Expr -> M trav Expr
makeRecLam trav ann f params tyBody eBody = do
  spanInFile <- askSpanInFile ann
  (labelOpt0, x0, ty0, paramsRest) <-
    case params of
      MandatoryBinder labelOpt0' (x0', ty0') : paramsRest' -> pure (labelOpt0', x0', ty0', paramsRest')
      OmissibleBinder {} : _ -> analysisError trav $ LetRecParamsCannotStartWithImplicit spanInFile
      InferableBinder {} : _ -> analysisError trav $ LetRecParamsCannotStartWithImplicit spanInFile
      [] -> analysisError trav $ LetRecRequiresNonEmptyParams spanInFile
  let (eRest, tyRest) = foldr go (eBody, tyBody) paramsRest
  let annTyRec =
        -- TODO (enhance): give better code position
        let Expr loc1 _ = ty0
            Expr loc2 _ = eBody
         in mergeSpan loc1 loc2
  let tyRec = Expr annTyRec (TyArrow labelOpt0 (Just x0, ty0) tyRest)
  pure $ Expr ann (Lam (Just (f, tyRec)) labelOpt0 (x0, ty0) eRest)
  where
    go :: LamBinder -> (Expr, TypeExpr) -> (Expr, TypeExpr)
    go (MandatoryBinder labelOpt (x, ty@(Expr loc1 _))) (eAcc@(Expr loc2 _), tyAcc) = do
      let ann' = mergeSpan loc1 loc2 -- TODO (enhance): give better code position
      let eAcc' = Expr ann' (Lam Nothing labelOpt (x, ty) eAcc)
      let tyAcc' = Expr ann' (TyArrow labelOpt (Just x, ty) tyAcc)
      (eAcc', tyAcc')
    go (OmissibleBinder label (x, ty@(Expr loc1 _))) (eAcc@(Expr loc2 _), tyAcc) = do
      let ann' = mergeSpan loc1 loc2 -- TODO (enhance): give better code position
      let eAcc' = Expr ann' (LamOms label (x, ty) eAcc)
      let tyAcc' = Expr ann' (TyOmsArrow label (Just x, ty) tyAcc)
      (eAcc', tyAcc')
    go (InferableBinder (x, ty@(Expr loc1 _))) (eAcc@(Expr loc2 _), tyAcc) = do
      let ann' = mergeSpan loc1 loc2 -- TODO (enhance): give better code position
      let eAcc' = Expr ann' (LamInf (x, ty) eAcc)
      let tyAcc' = Expr ann' (TyInfArrow (x, ty) tyAcc)
      (eAcc', tyAcc')

analysisError :: trav -> AnalysisError -> M trav a
analysisError = raiseError

askSpanInFile :: Span -> M trav SpanInFile
askSpanInFile loc = do
  AnalysisConfig {sourceSpec} <- askConfig
  pure $ getSpanInFile sourceSpec loc

enhanceBIType :: (bt -> BindingTime) -> (tv -> BITypeVar) -> BITypeF bt tv -> BIType
enhanceBIType enhBt enhBitv (BIType bt bityMain) =
  BIType (enhBt bt) $
    case bityMain of
      BITyVar bitv -> BITyVar (enhBitv bitv)
      BITyBase bityBaseArgs -> BITyBase (map fBIType bityBaseArgs)
      BITyProduct bitys -> BITyProduct (fmap fBIType bitys)
      BITyArrow bity1 bity2 -> BITyArrow (fBIType bity1) (fBIType bity2)
      BITyOmsArrow label bity1 bity2 -> BITyOmsArrow label (fBIType bity1) (fBIType bity2)
      BITyInfArrow bity1 bity2 -> BITyInfArrow (fBIType bity1) (fBIType bity2)
  where
    fBIType = enhanceBIType enhBt enhBitv

extractConstraintsFromLiteral :: trav -> BindingTimeEnv -> (BindingTime, Span) -> Literal Expr -> M trav (Literal BExpr, [BIType], [Constraint Span])
extractConstraintsFromLiteral trav btenv (btLit, annLit) = \case
  LitInt n ->
    pure (LitInt n, [], [])
  LitFloat r ->
    pure (LitFloat r, [], [])
  LitUnit ->
    pure (LitUnit, [], [])
  LitBool b ->
    pure (LitBool b, [], [])
  LitString t ->
    pure (LitString t, [], [])
  LitList es ->
    case es of
      [] -> do
        bitv <- freshBITypeVar
        let bity = BIType btLit (BITyVar bitv)
        pure (LitList [], [bity], [])
      eFirst : esTail -> do
        (eFirst', bityFirst@(BIType btElem _), constraintsFirst) <- extractConstraintsFromExpr trav btenv eFirst
        let constraintsLit = [CLeq annLit btLit btElem]
        (eAcc', constraintsAcc) <-
          foldM
            ( \(eAcc', constraintsAcc) e@(Expr ann _) -> do
                (e', bity, constraints) <- extractConstraintsFromExpr trav btenv e
                constraintsEq <- makeConstraintsFromBITypeEquation trav ann bityFirst bity
                pure (e' : eAcc', constraintsEq : constraints : constraintsAcc)
            )
            ([], [])
            esTail
        let es' = eFirst' : reverse eAcc'
        let constraints = constraintsLit ++ constraintsFirst ++ concat (reverse constraintsAcc)
        pure (LitList es', [bityFirst], constraints)
  LitVec ns ->
    pure (LitVec ns, [], [])
  LitMat nss ->
    pure (LitMat nss, [], [])

findVal :: BindingTimeEnv -> [ModuleName] -> Var -> Maybe BindingTimeValueEntry
findVal = go
  where
    go btenv ms x =
      case ms of
        [] ->
          Env.findVal x btenv
        m : ms' -> do
          BTModule btenv' <- Env.findModule m btenv
          go btenv' ms' x

openModule :: trav -> SpanInFile -> ModuleName -> BindingTimeEnv -> M trav BindingTimeEnv
openModule trav spanInFile m btenv =
  case Env.findModule m btenv of
    Just (BTModule btenv') -> pure $ Env.union btenv' btenv
    Nothing -> analysisError trav $ UnboundModule spanInFile m

makeInstantiationMap :: Set BITypeBoundVar -> M trav (Map BITypeBoundVar BITypeVar)
makeInstantiationMap =
  foldM
    ( \instantiationMap boundVar -> do
        bitv <- freshBITypeVar
        pure $ Map.insert boundVar bitv instantiationMap
    )
    Map.empty

collectTypeArgs :: trav -> Span -> ExprMain -> M trav ((Span, [ModuleName], TypeName), [Expr])
collectTypeArgs trav locApp = go locApp
  where
    go loc = \case
      App (Expr loc' eFunMain) Nothing eArg -> do
        (tyName, eArgs) <- go loc' eFunMain
        pure (tyName, eArgs ++ [eArg])
      Constructor (mods, tyName) -> do
        pure ((loc, mods, tyName), [])
      _ -> do
        spanInFile <- askSpanInFile locApp
        analysisError trav $ InvalidSyntaxAsTypeExpr spanInFile

extractConstraintsFromVar :: trav -> BindingTimeEnv -> BindingTime -> Span -> [Var] -> Var -> M trav (Var, BIType, [Constraint Span])
extractConstraintsFromVar trav btenv bt ann ms x = do
  spanInFile <- askSpanInFile ann
  case findVal btenv ms x of
    Nothing ->
      analysisError trav $ UnboundVar spanInFile ms x
    Just (BTValBuiltInPersistent x' biptyVoid) -> do
      let BIPolyType binders bityVoid = biptyVoid
      instantiationMap <- makeInstantiationMap binders
      let bity =
            enhanceBIType
              (\() -> bt)
              ( \boundVar ->
                  case Map.lookup boundVar instantiationMap of
                    Nothing -> error "bug: extractConstraintsFromExpr, not found"
                    Just bitv -> bitv
              )
              bityVoid
      pure (x', bity, [])
    Just (BTValBuiltInFixed0 x' biptyVoid) -> do
      let BIPolyType binders bityVoid = biptyVoid
      instantiationMap <- makeInstantiationMap binders
      let bity =
            enhanceBIType
              BTConst
              ( \boundVar ->
                  case Map.lookup boundVar instantiationMap of
                    Nothing -> error "bug: extractConstraintsFromExpr, not found"
                    Just bitv -> bitv
              )
              bityVoid
      pure (x', bity, [CEqual ann bt (BTConst BT0)])
    Just (BTValBuiltInFixed1 x' bityVoid) -> do
      let bity = enhanceBIType BTConst absurd bityVoid
      pure (x', bity, [CEqual ann bt (BTConst BT1)])
    Just (BTValLocallyBound bt' bity) ->
      pure (x, bity, [CEqual ann bt bt'])

extractConstraintsFromExpr :: trav -> BindingTimeEnv -> Expr -> M trav (BExpr, BIType, [Constraint Span])
extractConstraintsFromExpr trav btenv (Expr ann exprMain) = do
  btv <- freshBindingTimeVar
  let bt = BTVar btv
  spanInFile <- askSpanInFile ann
  case exprMain of
    Literal lit -> do
      (lit', bityBaseArgs, constraints) <- extractConstraintsFromLiteral trav btenv (bt, ann) lit
      pure (BExpr (bt, ann) (BLiteral lit'), BIType bt (BITyBase bityBaseArgs), constraints)
    Constructor _ ->
      error "TODO: extractConstraintsFromExpr, Constructor"
    Var (ms, x) -> do
      (x', bity, constraints) <- extractConstraintsFromVar trav btenv bt ann ms x
      pure (BExpr (bt, ann) (BVar (ms, x')), bity, constraints)
    Lam Nothing labelOpt (x1, btye1) e2 -> do
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr trav (Env.addVal x1 (BTValLocallyBound bt bity1) btenv) e2
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = BExpr (bt, ann) (BLam Nothing labelOpt (x1, btye1') e2')
      pure (e', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    Lam (Just (f, btyeRec)) labelOpt (x1, btye1) e2 -> do
      -- Not confident. TODO (theory): check the validity of the following
      (btyeRec', bityRec, constraintsRec) <- extractConstraintsFromTypeExpr trav btenv btyeRec
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr
          trav
          (Env.addVal x1 (BTValLocallyBound bt bity1) (Env.addVal f (BTValLocallyBound bt bityRec) btenv))
          e2
      let bitySynth = BIType bt (BITyArrow bity1 bity2)
      constraintsEq <- makeConstraintsFromBITypeEquation trav ann bitySynth bityRec
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = BExpr (bt, ann) (BLam (Just (f, btyeRec')) labelOpt (x1, btye1') e2')
      pure (e', bitySynth, constraintsRec ++ constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
    App e1 labelOpt e2 -> do
      (e1WithoutOpts, bity1WithoutOpts, constraints1) <- extractConstraintsFromExpr trav btenv e1
      let (e1', bity1@(BIType bt1 bityMain1)) = appendOmittedImplicitArguments e1WithoutOpts bity1WithoutOpts
      (e2', bity2, constraints2) <- extractConstraintsFromExpr trav btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyArrow bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity2 bity11
            pure (bity12, constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
          _ -> do
            let Expr ann1 _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError trav $ NotAFunction spanInFile1 bity1
      pure (BExpr (bt, ann) (BApp e1' labelOpt e2'), bity, constraints)
    Product e1 rest -> do
      extractConstraintsFromExpr trav btenv $
        foldl'
          ( \eAcc@(Expr annAcc _) ((annOp, op), eArg@(Expr annArg _)) ->
              Expr (mergeSpan annAcc annArg) $
                App
                  (Expr (mergeSpan annAcc annOp) (App (Expr annOp (Var ([], op))) Nothing eAcc))
                  Nothing
                  eArg
          )
          e1
          rest
    LetIn x params tyeBodyOpt eBody e2 -> do
      let e1 = makeLam params tyeBodyOpt eBody
      -- Not confident. TODO (theory): check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr trav (Env.addVal x (BTValLocallyBound bt bity1) btenv) e2
      constraints0 <-
        case tyeBodyOpt of
          Just tye0 -> do
            (_btye0', bity0, constraints0') <- extractConstraintsFromTypeExpr trav btenv tye0
            constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity0 (reconstructBodyType params bity1)
            pure $ constraints0' ++ constraintsEq
          Nothing ->
            pure []
      let e' = BExpr (bt, ann) (BLetIn x e1' e2')
      pure (e', bity2, constraints0 ++ constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    LetRecIn x params tye eBody e2 -> do
      e1 <- makeRecLam trav ann x params tye eBody
      -- Not confident. TODO (theory): check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr trav (Env.addVal x (BTValLocallyBound bt bity1) btenv) e2
      let e' = BExpr (bt, ann) (BLetIn x e1' e2')
      pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    LetTupleIn xs e1 e2 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr trav btenv e1
      case bityMain1 of
        BITyProduct bitys -> do
          case TwoOrMore.zipExact xs bitys of
            Just zipped -> do
              -- Not confident. TODO (theory): check the validity of the following
              (e2', bity2@(BIType bt2 _), constraints2) <- do
                let btenv2 =
                      foldl
                        ( \btenv' (x, bityElem@(BIType btElem _)) ->
                            Env.addVal x (BTValLocallyBound btElem bityElem) btenv'
                        )
                        btenv
                        zipped
                extractConstraintsFromExpr trav btenv2 e2
              let e' = BExpr (bt, ann) (BLetTupleIn xs e1' e2')
              pure (e', bity2, constraints1 ++ constraints2 ++ [CEqual ann bt bt1, CLeq ann bt bt2])
            Nothing ->
              analysisError trav $ TupleLengthMismatch spanInFile xs bitys
        _ -> do
          let Expr ann1 _ = e1
          spanInFile1 <- askSpanInFile ann1
          analysisError trav $ NotATuple spanInFile1 bity1
    LetOpenIn m e1 -> do
      (e1', bity1@(BIType bt1 _), constraints) <- do
        btenv' <- openModule trav spanInFile m btenv
        extractConstraintsFromExpr trav btenv' e1
      pure (BExpr (bt, ann) (BLetOpenIn m e1'), bity1, constraints ++ [CEqual ann bt bt1])
    Sequential e1 e2 -> do
      -- Not confident. TODO (theory): check the validity of the following
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromExpr trav btenv e2
      case bityMain1 of
        BITyBase [] -> do
          let e' = BExpr (bt, ann) (BSequential e1' e2')
          pure (e', bity2, constraints1 ++ constraints2 ++ [CEqual ann bt bt1, CLeq ann bt bt2])
        _ -> do
          let Expr ann1 _ = e1
          spanInFile1 <- askSpanInFile ann1
          analysisError trav $ NotABase spanInFile1 bity1
    Tuple es -> do
      -- Not confident. TODO (theory): check the validity of the following
      triples <- mapM (extractConstraintsFromExpr trav btenv) es
      let e' = BExpr (bt, ann) (BTuple (fmap (\(eElem, _, _) -> eElem) triples))
      let bity = BIType bt (BITyProduct (fmap (\(_, bityElem, _) -> bityElem) triples))
      let constraints =
            concatMap
              (\(_, BIType btElem _, constraintsElem) -> CLeq ann bt btElem : constraintsElem)
              triples
      pure (e', bity, constraints)
    IfThenElse e0 e1 e2 -> do
      (e0', bity0@(BIType bt0 bityMain0), constraints0) <- extractConstraintsFromExpr trav btenv e0
      case bityMain0 of
        BITyBase [] -> do
          (e1', bity1, constraints1) <- extractConstraintsFromExpr trav btenv e1
          (e2', bity2, constraints2) <- extractConstraintsFromExpr trav btenv e2
          let e' = BExpr (bt, ann) (BIfThenElse e0' e1' e2')
          constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity1 bity2
          pure (e', bity1, constraints0 ++ constraints1 ++ constraints2 ++ constraintsEq ++ [CEqual ann bt bt0])
        _ -> do
          let Expr ann0 _ = e0
          spanInFile0 <- askSpanInFile ann0
          analysisError trav $ NotABase spanInFile0 bity0
    As e1 tye2 -> do
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (btye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr trav btenv tye2
      constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity1 bity2
      let constraints = constraints1 ++ constraints2 ++ constraintsEq ++ [CLeq ann bt bt1, CLeq ann bt bt2]
      pure (BExpr (bt, ann) (BAs e1' btye2'), bity2, constraints)
    LamOms label (x1, btye1) e2 -> do
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr trav (Env.addVal x1 (BTValLocallyBound bt bity1) btenv) e2
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = BExpr (bt, ann) (BLamOms label (x1, btye1') e2')
      pure (e', BIType bt (BITyOmsArrow label bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    AppOms e1 label e2 -> do
      (e1WithoutOpts, bity1WithoutOpts, constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e1', (bt1, bity11, bity12)) <-
        appendOmittedImplicitArgumentsBeforeOms trav ann label e1WithoutOpts bity1WithoutOpts
      (e2', bity2, constraints2) <- extractConstraintsFromExpr trav btenv e2
      constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity2 bity11
      let constraints = constraints1 ++ constraints2 ++ constraintsEq ++ [CEqual ann bt bt1]
      pure (BExpr (bt, ann) (BAppOms e1' label e2'), bity12, constraints)
    LamInf (x1, btye1) e2 -> do
      (btye1', bity1, constraints1) <- extractConstraintsFromTypeExpr trav btenv btye1
      (e2', bity2, constraints2) <-
        extractConstraintsFromExpr trav (Env.addVal x1 (BTValLocallyBound bt bity1) btenv) e2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let e' = BExpr (bt, ann) (BLamInf (x1, btye1') e2')
      pure (e', BIType bt (BITyInfArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    AppInfGiven e1 e2 -> do
      (e1', bity1', constraints1) <- extractConstraintsFromExpr trav btenv e1
      let bity1@(BIType bt1 bityMain1) = skipOmittedArgumentsBeforeInf bity1'
      (e2', bity2, constraints2) <- extractConstraintsFromExpr trav btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyInfArrow bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity2 bity11
            pure (bity12, constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
          _ -> do
            let Expr ann1 _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError trav $ NotAnOptFunction spanInFile1 bity1
      pure (BExpr (bt, ann) (BAppInfGiven e1' e2'), bity, constraints)
    AppInfOmitted e1 -> do
      (e1', bity1', constraints1) <- extractConstraintsFromExpr trav btenv e1
      let bity1@(BIType bt1 bityMain1) = skipOmittedArgumentsBeforeInf bity1'
      (bity, constraints) <-
        case bityMain1 of
          BITyInfArrow _bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            pure (bity12, constraints1 ++ constraints)
          _ -> do
            let Expr ann1 _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError trav $ NotAnOptFunction spanInFile1 bity1
      pure (BExpr (bt, ann) (BAppInfOmitted e1'), bity, constraints)
    (TyArrow {}; TyOmsArrow {}; TyInfArrow {}; TyRefinement {}) ->
      analysisError trav $ InvalidSyntaxAsExpr spanInFile

skipOmittedArgumentsBeforeInf :: BIType -> BIType
skipOmittedArgumentsBeforeInf bity@(BIType _bt bityMain) =
  case bityMain of
    BITyOmsArrow _label _bity1 bity2 -> skipOmittedArgumentsBeforeInf bity2
    _ -> bity

appendOmittedImplicitArgumentsBeforeOms :: trav -> Span -> Label -> BExpr -> BIType -> M trav (BExpr, (BindingTime, BIType, BIType))
appendOmittedImplicitArgumentsBeforeOms trav loc labelReq = go
  where
    go e@(BExpr (_, ann) _) (BIType bt bityMain) = do
      case bityMain of
        BITyOmsArrow label bity1 bity2 ->
          if label == labelReq
            then pure (e, (bt, bity1, bity2))
            else go e bity2
        BITyInfArrow _bity1 bity2 ->
          -- TODO (enhance): give better location than `ann`
          go (BExpr (BTConst BT0, ann) (BAppInfOmitted e)) bity2
        _ -> do
          spanInFile <- askSpanInFile loc
          analysisError trav $ NoOmissibleParameter spanInFile labelReq

appendOmittedImplicitArguments :: BExpr -> BIType -> (BExpr, BIType)
appendOmittedImplicitArguments e@(BExpr (_, ann) _) bity@(BIType _bt bityMain) =
  case bityMain of
    BITyOmsArrow _label _bity1 bity2 ->
      appendOmittedImplicitArguments e bity2
    BITyInfArrow _bity1 bity2 ->
      -- TODO (enhance): give better location than `ann`
      appendOmittedImplicitArguments (BExpr (BTConst BT0, ann) (BAppInfOmitted e)) bity2
    _ ->
      (e, bity)

unwrapBITypeVar :: BITypeMain -> M trav (Either BITypeVar BITypeMain)
unwrapBITypeVar = \case
  BITyVar bitv -> do
    AnalysisState {solution} <- getState
    pure $
      case Map.lookup bitv solution of
        Nothing -> Left bitv
        Just bityMain -> Right bityMain
  bityMain ->
    pure $ Right bityMain

occurs :: BITypeVar -> BITypeMain -> Bool
occurs bitv = goMain
  where
    goMain = \case
      BITyVar bitv' -> bitv' == bitv
      BITyBase bitys -> any go bitys
      BITyProduct bitys -> any go bitys
      BITyArrow bity1 bity2 -> go bity1 || go bity2
      BITyOmsArrow _label bity1 bity2 -> go bity1 || go bity2
      BITyInfArrow bity1 bity2 -> go bity1 || go bity2
    go (BIType _bt bityMain) =
      goMain bityMain

makeConstraintsFromBITypeEquation :: forall trav. trav -> Span -> BIType -> BIType -> M trav [Constraint Span]
makeConstraintsFromBITypeEquation trav ann bity1' bity2' = go bity1' bity2'
  where
    go :: BIType -> BIType -> M trav [Constraint Span]
    go bity1@(BIType bt1 bityMain1') bity2@(BIType bt2 bityMain2') = do
      unwrapped1 <- unwrapBITypeVar bityMain1'
      unwrapped2 <- unwrapBITypeVar bityMain2'
      ([CEqual ann bt1 bt2] ++)
        <$> case (unwrapped1, unwrapped2) of
          (Right nonvarBityMain1, Right nonvarBityMain2) ->
            goNonvar nonvarBityMain1 nonvarBityMain2
          (Left bitv1, Left bitv2) -> do
            if bitv1 == bitv2
              then pure ()
              else solve bitv1 bityMain2'
            pure []
          (Left bitv1, Right nonvarBityMain2) ->
            if bitv1 `occurs` nonvarBityMain2
              then do
                spanInFile <- askSpanInFile ann
                analysisError trav $ BITypeInclusionLeft spanInFile bity1' bity2' bitv1 bity2
              else do
                solve bitv1 nonvarBityMain2
                pure []
          (Right nonvarBityMain1, Left bitv2) ->
            if bitv2 `occurs` nonvarBityMain1
              then do
                spanInFile <- askSpanInFile ann
                analysisError trav $ BITypeInclusionRight spanInFile bity1' bity2' bity1 bitv2
              else do
                solve bitv2 nonvarBityMain1
                pure []
      where
        solve :: BITypeVar -> BITypeMain -> M trav ()
        solve bitv bityMain = do
          st@AnalysisState {solution} <- getState
          putState $ st {solution = Map.insert bitv bityMain solution}

        goNonvar :: BITypeMain -> BITypeMain -> M trav [Constraint Span]
        goNonvar bityMain1 bityMain2 =
          case (bityMain1, bityMain2) of
            (BITyBase bityBaseArgs1, BITyBase bityBaseArgs2) ->
              case zipExactMay bityBaseArgs1 bityBaseArgs2 of
                Nothing -> do
                  spanInFile <- askSpanInFile ann
                  analysisError trav $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2
                Just zipped -> do
                  concat <$> mapM (uncurry go) zipped
            (BITyProduct bitys1, BITyProduct bitys2) -> do
              case zipExactMay (TwoOrMore.toList bitys1) (TwoOrMore.toList bitys2) of
                Just zipped ->
                  concat <$> mapM (uncurry go) zipped
                Nothing -> do
                  spanInFile <- askSpanInFile ann
                  analysisError trav $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2
            (BITyArrow bity11 bity12, BITyArrow bity21 bity22) -> do
              constraints1 <- go bity11 bity21
              constraints2 <- go bity12 bity22
              pure $ constraints1 ++ constraints2
            (BITyOmsArrow label1 bity11 bity12, BITyOmsArrow label2 bity21 bity22) -> do
              if label1 == label2
                then do
                  constraints1 <- go bity11 bity21
                  constraints2 <- go bity12 bity22
                  pure $ constraints1 ++ constraints2
                else do
                  spanInFile <- askSpanInFile ann
                  analysisError trav $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2
            (BITyInfArrow bity11 bity12, BITyInfArrow bity21 bity22) -> do
              constraints1 <- go bity11 bity21
              constraints2 <- go bity12 bity22
              pure $ constraints1 ++ constraints2
            (_, _) -> do
              spanInFile <- askSpanInFile ann
              analysisError trav $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2

extractConstraintsFromExprArgsForType :: trav -> BindingTimeEnv -> BindingTime -> Span -> [(Expr, BIType)] -> M trav ([BExpr], [Constraint Span])
extractConstraintsFromExprArgsForType trav btenv bt ann argsWithBityReq = do
  pairs <-
    mapM
      ( \(e, bityReq) -> do
          let Expr ann' _ = e
          (e', bityGot, constraints') <- extractConstraintsFromExpr trav btenv e
          constraintsEq <- makeConstraintsFromBITypeEquation trav ann' bityGot bityReq
          pure (e', constraints' ++ constraintsEq)
      )
      argsWithBityReq
  let args' = map fst pairs
  let constraints' = concatMap snd pairs
  let constraints = constraints' ++ [CEqual ann bt (BTConst BT1)]
  pure (args', constraints)

extractConstraintsFromTypeExpr :: trav -> BindingTimeEnv -> TypeExpr -> M trav (BTypeExpr, BIType, [Constraint Span])
extractConstraintsFromTypeExpr trav btenv (Expr ann typeExprMain) = do
  btv <- freshBindingTimeVar
  let bt = BTVar btv
  spanInFile <- askSpanInFile ann
  case typeExprMain of
    Constructor (mods, tyName) ->
      case mods of
        [] -> do
          constraints <-
            case tyName of
              "Nat" ->
                pure [CEqual ann bt (BTConst BT0)]
              _ ->
                case Staged.validatePrimBaseType tyName of
                  Just _tyPrimBase -> pure []
                  Nothing -> analysisError trav $ UnknownTypeOrInvalidArity spanInFile mods tyName 0
          let tye' = BTypeExpr (bt, ann) (BTyName (ann, tyName) [])
          pure (tye', BIType bt (BITyBase []), constraints)
        _ : _ ->
          analysisError trav $ UnknownTypeOrInvalidArity spanInFile mods tyName 0
    App _ labelOpt _ -> do
      () <-
        case labelOpt of
          Nothing -> pure ()
          Just _ -> analysisError trav $ InvalidSyntaxAsTypeExpr spanInFile
      ((locQualName, mods, tyName), args) <- collectTypeArgs trav ann typeExprMain
      (args', bityBaseArgs, constraints) <-
        case (mods, tyName, args) of
          ([], "List", [tye]) -> do
            (tyeElem, bity@(BIType btElem _), cs) <- extractConstraintsFromTypeExpr trav btenv tye
            pure ([BTypeExprArg tyeElem], [bity], cs ++ [CLeq ann bt btElem])
          ([], "Vec", [e]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType trav btenv bt ann [(e, bityNat)]
            pure (map BExprArg exprArgs, [], cs)
          ([], "Mat", [e1, e2]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType trav btenv bt ann [(e1, bityNat), (e2, bityNat)]
            pure (map BExprArg exprArgs, [], cs)
          ([], "Tensor", [eList]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType trav btenv bt ann [(eList, bityNatList)]
            pure (map BExprArg exprArgs, [], cs)
          ([], "Dataset", [e1, e2, e3, e4]) -> do
            (exprArgs, cs) <-
              extractConstraintsFromExprArgsForType trav btenv bt ann $
                [(e1, bityNat), (e2, bityNat), (e3, bityNatList), (e4, bityNatList)]
            pure (map BExprArg exprArgs, [], cs)
          ([], "Lstm", [eInputSize, eHiddenSize]) -> do
            (exprArgs, cs) <-
              extractConstraintsFromExprArgsForType trav btenv bt ann $
                [(eInputSize, bityNat), (eHiddenSize, bityNat)]
            pure (map BExprArg exprArgs, [], cs)
          ([], "TextHelper", [eLabels]) -> do
            (exprArgs, cs) <-
              extractConstraintsFromExprArgsForType trav btenv bt ann $
                [(eLabels, bityNat)]
            pure (map BExprArg exprArgs, [], cs)
          (_, _, _) ->
            analysisError trav $ UnknownTypeOrInvalidArity spanInFile mods tyName (length args)
      let tye' = BTypeExpr (bt, ann) (BTyName (locQualName, tyName) args')
      pure (tye', BIType bt (BITyBase bityBaseArgs), constraints)
    TyArrow labelOpt (x1opt, tye1) tye2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      case x1opt of
        Nothing -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr trav btenv tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = BTypeExpr (bt, ann) (BTyArrow labelOpt (Nothing, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
        Just x1 -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <-
            extractConstraintsFromTypeExpr trav (Env.addVal x1 (BTValLocallyBound bt bity1) btenv) tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = BTypeExpr (bt, ann) (BTyArrow labelOpt (Just x1, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyOmsArrow label (x1opt, tye1) tye2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      case x1opt of
        Nothing -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr trav btenv tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = BTypeExpr (bt, ann) (BTyOmsArrow label (Nothing, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
        Just x1 -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <-
            extractConstraintsFromTypeExpr trav (Env.addVal x1 (BTValLocallyBound bt bity1) btenv) tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = BTypeExpr (bt, ann) (BTyOmsArrow label (Just x1, tye1') tye2')
          pure (tye', BIType bt (BITyOmsArrow label bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyInfArrow (x1, tye1) tye2 -> do
      (tye1', bity1, constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      (tye2', bity2, constraints2) <-
        extractConstraintsFromTypeExpr trav (Env.addVal x1 (BTValLocallyBound bt bity1) btenv) tye2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let tye' = BTypeExpr (bt, ann) (BTyInfArrow (x1, tye1') tye2')
      pure (tye', BIType bt (BITyInfArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyRefinement x tye1 e2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      (e2', BIType bt2 _, constraints2) <-
        extractConstraintsFromExpr trav (Env.addVal x (BTValLocallyBound bt bity1) btenv) e2
      let constraints = [CEqual ann bt (BTConst BT0), CEqual ann bt1 (BTConst BT0), CEqual ann bt2 (BTConst BT0)]
      let tye' = BTypeExpr (bt, ann) (BTyRefinement x tye1' e2')
      pure (tye', bity1, constraints1 ++ constraints2 ++ constraints)
    Product tye1 rest -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      quadsRest <-
        mapM
          ( \((locAster, op), tye) ->
              case op of
                "*" -> (locAster,) <$> extractConstraintsFromTypeExpr trav btenv tye
                _ -> analysisError trav $ InvalidSyntaxAsTypeExpr spanInFile
          )
          rest
      let constraintsRest =
            concatMap
              ( \(_locAster, (_tye', BIType bt' _, constraints')) ->
                  CLeq ann bt bt' : constraints'
              )
              (NonEmpty.toList quadsRest)
      let constraints = CLeq ann bt bt1 : constraints1 ++ constraintsRest
      let rest' = fmap (\(locAster, (tye', _, _)) -> (locAster, tye')) quadsRest
      let tye' = BTypeExpr (bt, ann) (BTyProduct tye1' rest')
      let bitysRest = fmap (\(_, (_, bity, _)) -> bity) quadsRest
      pure (tye', BIType bt (BITyProduct (TwoOrMore.make1 bity1 bitysRest)), constraints)
    (Literal {}; Var {}; Lam {}; LetIn {}; LetRecIn {}; LetTupleIn {}; LetOpenIn {}; Sequential {}; Tuple {}; IfThenElse {}; As {}; LamOms {}; AppOms {}; LamInf {}; AppInfGiven {}; AppInfOmitted {}) ->
      analysisError trav $ InvalidSyntaxAsTypeExpr spanInFile
  where
    bityNat :: BIType
    bityNat = BIType (BTConst BT0) (BITyBase [])

    bityNatList :: BIType
    bityNatList = BIType (BTConst BT0) (BITyBase [bityNat])

run :: SourceSpec -> BindingTimeEnv -> Expr -> Either AnalysisError (BExpr, [Constraint Span])
run sourceSpec btenv e = do
  let (result, _finalState) = Elaborator.run (extractConstraintsFromExpr () btenv e) analysisConfig initialState
  (be', _bity, constraints) <- mapLeft fst result
  pure (be', constraints)
  where
    analysisConfig = AnalysisConfig {sourceSpec}
