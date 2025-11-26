module Surface.BindingTime.Analyzer
  ( AnalysisError (..),
    run,
  )
where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Foldable (foldrM)
import Data.Function ((&))
import Data.Map qualified as Map
import Safe.Exact (zipExactMay)
import Staged.Syntax qualified as Staged
import Surface.BindingTime.AnalysisError
import Surface.BindingTime.Constraint
import Surface.BindingTime.Core
import Surface.Syntax
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.TokenUtil
import Prelude hiding (succ)

initialState :: BindingTimeVar
initialState = BindingTimeVar 0

succ :: BindingTimeVar -> BindingTimeVar
succ (BindingTimeVar n) = BindingTimeVar (n + 1)

type Assigner a = State BindingTimeVar a

fresh :: Assigner BindingTimeVar
fresh = do
  btv <- get
  put (succ btv)
  pure btv

-- TODO (enhance): merge this function into `extractConstraintsFromExpr`
assignBindingTimeVarToExpr :: Expr -> Assigner BExpr
assignBindingTimeVarToExpr (Expr ann exprMain) = do
  btv <- fresh
  Expr (BTVar btv, ann)
    <$> case exprMain of
      Literal lit ->
        Literal <$> mapMLiteral assignBindingTimeVarToExpr lit
      Var x ->
        pure $ Var x
      Lam Nothing (x, ty) e -> do
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ Lam Nothing (x, bty) be
      Lam (Just (f, tyRec)) (x, ty) e -> do
        btyRec <- assignBindingTimeVarToTypeExpr tyRec
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ Lam (Just (f, btyRec)) (x, bty) be
      App e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ App be1 be2
      LetIn x params eBody e2 -> do
        be1 <- makeLam params eBody
        be2 <- assignBindingTimeVarToExpr e2
        pure $ LetIn x [] be1 be2
      LetRecIn f params tyeBody eBody e2 -> do
        be1 <- makeRecLam f params tyeBody eBody
        be2 <- assignBindingTimeVarToExpr e2
        pure $ LetIn f [] be1 be2
      LetTupleIn xL xR e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ LetTupleIn xL xR be1 be2
      LetOpenIn m e -> do
        be <- assignBindingTimeVarToExpr e
        pure $ LetOpenIn m be
      Sequential e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ Sequential be1 be2
      Tuple e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ Tuple be1 be2
      IfThenElse e0 e1 e2 -> do
        be0 <- assignBindingTimeVarToExpr e0
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ IfThenElse be0 be1 be2
      As e1 ty2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        btye2 <- assignBindingTimeVarToTypeExpr ty2
        pure $ As be1 btye2
      LamOpt (x, ty) e -> do
        bty <- assignBindingTimeVarToTypeExpr ty
        be <- assignBindingTimeVarToExpr e
        pure $ LamOpt (x, bty) be
      AppOptGiven e1 e2 -> do
        be1 <- assignBindingTimeVarToExpr e1
        be2 <- assignBindingTimeVarToExpr e2
        pure $ AppOptGiven be1 be2
      AppOptOmitted e1 -> do
        be1 <- assignBindingTimeVarToExpr e1
        pure $ AppOptOmitted be1

makeLam :: [LamBinder] -> Expr -> Assigner BExpr
makeLam params eBody = do
  beBody <- assignBindingTimeVarToExpr eBody
  foldrM go beBody params
  where
    go :: LamBinder -> BExpr -> Assigner BExpr
    go (MandatoryBinder (x, ty@(TypeExpr loc1 _))) be@(Expr (_, loc2) _) = do
      btv <- fresh
      let ann = mergeSpan loc1 loc2 -- TODO (enhance): give better range
      bty <- assignBindingTimeVarToTypeExpr ty
      pure $ Expr (BTVar btv, ann) (Lam Nothing (x, bty) be)
    go (OptionalBinder (x, ty@(TypeExpr loc1 _))) be@(Expr (_, loc2) _) = do
      btv <- fresh
      let ann = mergeSpan loc1 loc2 -- TODO (enhance): give better range
      bty <- assignBindingTimeVarToTypeExpr ty
      pure $ Expr (BTVar btv, ann) (LamOpt (x, bty) be)

makeRecLam :: Var -> [LamBinder] -> TypeExpr -> Expr -> Assigner BExpr
makeRecLam f params tyBody eBody = do
  beBody <- assignBindingTimeVarToExpr eBody
  (x0, ty0, paramsRest) <-
    case params of
      MandatoryBinder (x0', ty0') : paramsRest' -> pure (x0', ty0', paramsRest')
      OptionalBinder _ : _ -> error "TODO (error): makeLamRec, optional binder"
      [] -> error "TODO (error): makeLamRec, empty parameter sequence"
  btyBody <- assignBindingTimeVarToTypeExpr tyBody
  (beRest, btyRest) <- foldrM go (beBody, btyBody) paramsRest
  bty0 <- assignBindingTimeVarToTypeExpr ty0
  btv <- fresh
  let ann =
        -- TODO (enhance): give better code position
        let TypeExpr loc1 _ = ty0
            Expr loc2 _ = eBody
         in mergeSpan loc1 loc2
  let btyRec = TypeExpr (BTVar btv, ann) (TyArrow (Just x0, bty0) btyRest)
  pure $ Expr (BTVar btv, ann) (Lam (Just (f, btyRec)) (x0, bty0) beRest)
  where
    go :: LamBinder -> (BExpr, BTypeExpr) -> Assigner (BExpr, BTypeExpr)
    go (MandatoryBinder (x, ty@(TypeExpr loc1 _))) (beAcc@(Expr (_, loc2) _), btyAcc) = do
      btv <- fresh
      let ann = mergeSpan loc1 loc2 -- TODO (enhance): give better code position
      bty <- assignBindingTimeVarToTypeExpr ty
      let beAcc' = Expr (BTVar btv, ann) (Lam Nothing (x, bty) beAcc)
      let btyAcc' = TypeExpr (BTVar btv, ann) (TyArrow (Just x, bty) btyAcc)
      pure (beAcc', btyAcc')
    go (OptionalBinder (x, ty@(TypeExpr loc1 _))) (beAcc@(Expr (_, loc2) _), btyAcc) = do
      btv <- fresh
      let ann = mergeSpan loc1 loc2 -- TODO (enhance): give better code position
      bty <- assignBindingTimeVarToTypeExpr ty
      let beAcc' = Expr (BTVar btv, ann) (LamOpt (x, bty) beAcc)
      let btyAcc' = TypeExpr (BTVar btv, ann) (TyOptArrow (x, bty) btyAcc)
      pure (beAcc', btyAcc')

assignBindingTimeVarToTypeExpr :: TypeExpr -> Assigner BTypeExpr
assignBindingTimeVarToTypeExpr (TypeExpr ann typeExprMain) = do
  btv <- fresh
  TypeExpr (BTVar btv, ann)
    <$> case typeExprMain of
      TyName tyName args -> do
        bargs <- mapM assignBindingTimeVarToArgForType args
        pure $ TyName tyName bargs
      TyArrow (xOpt, ty1) ty2 -> do
        bty1 <- assignBindingTimeVarToTypeExpr ty1
        bty2 <- assignBindingTimeVarToTypeExpr ty2
        pure $ TyArrow (xOpt, bty1) bty2
      TyOptArrow (x, ty1) ty2 -> do
        bty1 <- assignBindingTimeVarToTypeExpr ty1
        bty2 <- assignBindingTimeVarToTypeExpr ty2
        pure $ TyOptArrow (x, bty1) bty2
      TyProduct ty1 ty2 -> do
        bty1 <- assignBindingTimeVarToTypeExpr ty1
        bty2 <- assignBindingTimeVarToTypeExpr ty2
        pure $ TyProduct bty1 bty2

assignBindingTimeVarToArgForType :: ArgForType -> Assigner BArgForType
assignBindingTimeVarToArgForType = \case
  ExprArg e -> ExprArg <$> assignBindingTimeVarToExpr e
  TypeArg tye -> TypeArg <$> assignBindingTimeVarToTypeExpr tye

newtype AnalysisConfig = AnalysisConfig
  { sourceSpec :: SourceSpec
  }

type M a = ReaderT AnalysisConfig (Either AnalysisError) a

analysisError :: AnalysisError -> M a
analysisError = lift . Left

askSpanInFile :: Span -> M SpanInFile
askSpanInFile loc = do
  AnalysisConfig {sourceSpec} <- ask
  pure $ getSpanInFile sourceSpec loc

enhanceBIType :: (bt -> BindingTime) -> BITypeF bt -> BIType
enhanceBIType enhBt (BIType bt bityMain) =
  BIType (enhBt bt) $
    case bityMain of
      BITyBase bityBaseArgs -> BITyBase (map fBIType bityBaseArgs)
      BITyProduct bity1 bity2 -> BITyProduct (fBIType bity1) (fBIType bity2)
      BITyArrow bity1 bity2 -> BITyArrow (fBIType bity1) (fBIType bity2)
      BITyOptArrow bity1 bity2 -> BITyOptArrow (fBIType bity1) (fBIType bity2)
  where
    fBIType = enhanceBIType enhBt

extractConstraintsFromLiteral :: BindingTimeEnv -> (BindingTime, Span) -> Literal BExpr -> M (Literal BExpr, [BIType], [Constraint Span])
extractConstraintsFromLiteral btenv (btLit, annLit) = \case
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
        let bity = error "TODO: generate fresh binding-time variable"
        pure (LitList [], [bity], [])
      eFirst : esTail -> do
        (eFirst', bityFirst@(BIType btElem _), constraintsFirst) <- extractConstraintsFromExpr btenv eFirst
        let constraintsLit = [CLeq annLit btLit btElem]
        (eAcc', constraintsAcc) <-
          foldM
            ( \(eAcc', constraintsAcc) e@(Expr (_, ann) _) -> do
                (e', bity, constraints) <- extractConstraintsFromExpr btenv e
                constraintsEq <- makeConstraintsFromBITypeEquation ann bityFirst bity
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

findVal :: BindingTimeEnv -> [Var] -> Var -> Maybe BindingTimeEnvEntry
findVal = go
  where
    go btenv ms x =
      case ms of
        [] ->
          Map.lookup x btenv
        m : ms' ->
          case Map.lookup m btenv of
            Just (EntryModule btenv') -> go btenv' ms' x
            _ -> Nothing

openModule :: SpanInFile -> Var -> BindingTimeEnv -> M BindingTimeEnv
openModule spanInFile m btenv =
  case Map.lookup m btenv of
    Just (EntryModule btenv') ->
      pure $ Map.union btenv' btenv
    _ ->
      analysisError $ NotAModule spanInFile m

extractConstraintsFromExpr :: BindingTimeEnv -> BExpr -> M (BExpr, BIType, [Constraint Span])
extractConstraintsFromExpr btenv (Expr (bt, ann) exprMain) = do
  spanInFile <- askSpanInFile ann
  case exprMain of
    Literal lit -> do
      (lit', bityBaseArgs, constraints) <- extractConstraintsFromLiteral btenv (bt, ann) lit
      pure (Expr (bt, ann) (Literal lit'), BIType bt (BITyBase bityBaseArgs), constraints)
    Var (ms, x) -> do
      (x', bity, constraints) <-
        case findVal btenv ms x of
          Nothing ->
            analysisError $ UnboundVar spanInFile ms x
          Just (EntryBuiltInPersistent x' bityVoid) ->
            pure (x', enhanceBIType (\() -> bt) bityVoid, [])
          Just (EntryBuiltInFixed x' btc' bityConst) ->
            pure (x', enhanceBIType BTConst bityConst, [CEqual ann bt (BTConst btc')])
          Just (EntryLocallyBound bt' bity) ->
            pure (x, bity, [CEqual ann bt bt'])
          Just (EntryModule _) ->
            analysisError $ NotAVal spanInFile ms x
      pure (Expr (bt, ann) (Var (ms, x')), bity, constraints)
    Lam Nothing (x1, btye1) e2 -> do
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) e2
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = Expr (bt, ann) (Lam Nothing (x1, btye1') e2')
      pure (e', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    Lam (Just (f, btyeRec)) (x1, btye1) e2 -> do
      -- Not confident. TODO: check the validity of the following
      (btyeRec', bityRec, constraintsRec) <- extractConstraintsFromTypeExpr btenv btyeRec
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr
          (Map.insert x1 (EntryLocallyBound bt bity1) (Map.insert f (EntryLocallyBound bt bityRec) btenv))
          e2
      let bitySynth = BIType bt (BITyArrow bity1 bity2)
      constraintsEq <- makeConstraintsFromBITypeEquation ann bitySynth bityRec
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = Expr (bt, ann) (Lam (Just (f, btyeRec')) (x1, btye1') e2')
      pure (e', bitySynth, constraintsRec ++ constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
    App e1 e2 -> do
      (e1WithoutOpts, bity1WithoutOpts, constraints1) <- extractConstraintsFromExpr btenv e1
      let (e1', bity1@(BIType bt1 bityMain1)) = appendOmittedOptionalArguments e1WithoutOpts bity1WithoutOpts
      (e2', bity2, constraints2) <- extractConstraintsFromExpr btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyArrow bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            constraintsEq <- makeConstraintsFromBITypeEquation ann bity2 bity11
            pure (bity12, constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
          _ -> do
            let Expr (_, ann1) _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError $ NotAFunction spanInFile1 bity1
      pure (Expr (bt, ann) (App e1' e2'), bity, constraints)
    LetIn _x (_ : _) _eBody _e2 ->
      error "Bug: Analyzer.extractConstraintsFromExpr, non-empty parameter sequence"
    LetIn x [] e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr (Map.insert x (EntryLocallyBound bt bity1) btenv) e2
      let e' = Expr (bt, ann) (LetIn x [] e1' e2')
      pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    LetRecIn _x _params _tye _eBody _e2 ->
      error "Bug: Analyzer.extractConstraintsFromExpr, LetRecIn"
    LetTupleIn xL xR e1 e2 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      case bityMain1 of
        BITyProduct bityL@(BIType btL _) bityR@(BIType btR _) -> do
          (e2', bity2@(BIType bt2 _), constraints2) <-
            extractConstraintsFromExpr
              (btenv & Map.insert xL (EntryLocallyBound btL bityL) & Map.insert xR (EntryLocallyBound btR bityR))
              e2
          let e' = Expr (bt, ann) (LetTupleIn xL xR e1' e2')
          pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
        _ -> do
          let Expr (_, ann1) _ = e1
          spanInFile1 <- askSpanInFile ann1
          analysisError $ NotATuple spanInFile1 bity1
    LetOpenIn m e1 -> do
      (e1', bity1@(BIType bt1 _), constraints) <- do
        btenv' <- openModule spanInFile m btenv
        extractConstraintsFromExpr btenv' e1
      pure (Expr (bt, ann) (LetOpenIn m e1'), bity1, constraints ++ [CEqual ann bt bt1])
    Sequential e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromExpr btenv e2
      case bityMain1 of
        BITyBase [] -> do
          let e' = Expr (bt, ann) (Sequential e1' e2')
          pure (e', bity2, constraints1 ++ constraints2 ++ [CEqual ann bt bt1, CLeq ann bt bt2])
        _ -> do
          let Expr (_, ann1) _ = e1
          spanInFile1 <- askSpanInFile ann1
          analysisError $ NotABase spanInFile1 bity1
    Tuple e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromExpr btenv e2
      let e' = Expr (bt, ann) (Sequential e1' e2')
      let bity = BIType bt (BITyProduct bity1 bity2)
      pure (e', bity, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    IfThenElse e0 e1 e2 -> do
      (e0', bity0@(BIType bt0 bityMain0), constraints0) <- extractConstraintsFromExpr btenv e0
      case bityMain0 of
        BITyBase [] -> do
          (e1', bity1, constraints1) <- extractConstraintsFromExpr btenv e1
          (e2', bity2, constraints2) <- extractConstraintsFromExpr btenv e2
          let e' = Expr (bt, ann) (IfThenElse e0' e1' e2')
          constraintsEq <- makeConstraintsFromBITypeEquation ann bity1 bity2
          pure (e', bity1, constraints0 ++ constraints1 ++ constraints2 ++ constraintsEq ++ [CEqual ann bt bt0])
        _ -> do
          let Expr (_, ann0) _ = e0
          spanInFile0 <- askSpanInFile ann0
          analysisError $ NotABase spanInFile0 bity0
    As e1 btye2 -> do
      (e1', bity1, constraints1) <- extractConstraintsFromExpr btenv e1
      (btye2', bity2, constraints2) <- extractConstraintsFromTypeExpr btenv btye2
      constraintsEq <- makeConstraintsFromBITypeEquation ann bity1 bity2
      pure (Expr (bt, ann) (As e1' btye2'), bity2, constraints1 ++ constraints2 ++ constraintsEq)
    LamOpt (x1, btye1) e2 -> do
      (btye1', bity1, constraints1) <- extractConstraintsFromTypeExpr btenv btye1
      (e2', bity2, constraints2) <-
        extractConstraintsFromExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) e2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let e' = Expr (bt, ann) (LamOpt (x1, btye1') e2')
      pure (e', BIType bt (BITyOptArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    AppOptGiven e1 e2 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (e2', bity2, constraints2) <- extractConstraintsFromExpr btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyOptArrow bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            constraintsEq <- makeConstraintsFromBITypeEquation ann bity2 bity11
            pure (bity12, constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
          _ -> do
            let Expr (_, ann1) _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError $ NotAnOptFunction spanInFile1 bity1
      pure (Expr (bt, ann) (AppOptGiven e1' e2'), bity, constraints)
    AppOptOmitted e1 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr btenv e1
      (bity, constraints) <-
        case bityMain1 of
          BITyOptArrow _bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            pure (bity12, constraints1 ++ constraints)
          _ -> do
            let Expr (_, ann1) _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError $ NotAnOptFunction spanInFile1 bity1
      pure (Expr (bt, ann) (AppOptOmitted e1'), bity, constraints)

appendOmittedOptionalArguments :: BExpr -> BIType -> (BExpr, BIType)
appendOmittedOptionalArguments e@(Expr (_, ann) _) bity@(BIType _bt bityMain) =
  case bityMain of
    BITyOptArrow _bity1 bity2 ->
      -- TODO (enhance): give better location than `ann`
      appendOmittedOptionalArguments (Expr (BTConst BT0, ann) (AppOptOmitted e)) bity2
    _ ->
      (e, bity)

makeConstraintsFromBITypeEquation :: Span -> BIType -> BIType -> M [Constraint Span]
makeConstraintsFromBITypeEquation ann bity1' bity2' = go bity1' bity2'
  where
    go :: BIType -> BIType -> M [Constraint Span]
    go bity1@(BIType bt1 bityMain1) bity2@(BIType bt2 bityMain2) =
      ([CEqual ann bt1 bt2] ++)
        <$> case (bityMain1, bityMain2) of
          (BITyBase bityBaseArgs1, BITyBase bityBaseArgs2) ->
            case zipExactMay bityBaseArgs1 bityBaseArgs2 of
              Nothing -> do
                spanInFile <- askSpanInFile ann
                analysisError $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2
              Just zipped -> do
                concat <$> mapM (uncurry go) zipped
          (BITyProduct bity11 bity12, BITyProduct bity21 bity22) -> do
            constraints1 <- go bity11 bity21
            constraints2 <- go bity12 bity22
            pure $ constraints1 ++ constraints2
          (BITyArrow bity11 bity12, BITyArrow bity21 bity22) -> do
            constraints1 <- go bity11 bity21
            constraints2 <- go bity12 bity22
            pure $ constraints1 ++ constraints2
          (BITyOptArrow bity11 bity12, BITyOptArrow bity21 bity22) -> do
            constraints1 <- go bity11 bity21
            constraints2 <- go bity12 bity22
            pure $ constraints1 ++ constraints2
          (_, _) -> do
            spanInFile <- askSpanInFile ann
            analysisError $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2

extractConstraintsFromExprArgsForType :: BindingTimeEnv -> BindingTime -> Span -> [(BExpr, BIType)] -> M ([BExpr], [Constraint Span])
extractConstraintsFromExprArgsForType btenv bt ann argsWithBityReq = do
  pairs <-
    mapM
      ( \(e, bityReq) -> do
          (e', bityGot, constraints') <- extractConstraintsFromExpr btenv e
          constraintsEq <- makeConstraintsFromBITypeEquation ann bityGot bityReq
          pure (e', constraints' ++ constraintsEq)
      )
      argsWithBityReq
  let args' = map fst pairs
  let constraints' = concatMap snd pairs
  let constraints = constraints' ++ [CEqual ann bt (BTConst BT1)]
  pure (args', constraints)

extractConstraintsFromTypeExpr :: BindingTimeEnv -> BTypeExpr -> M (BTypeExpr, BIType, [Constraint Span])
extractConstraintsFromTypeExpr btenv (TypeExpr (bt, ann) typeExprMain) = do
  spanInFile <- askSpanInFile ann
  case typeExprMain of
    TyName tyName args -> do
      (args', bityBaseArgs, constraints) <-
        case (tyName, args) of
          ("Nat", []) ->
            pure ([], [], [CEqual ann bt (BTConst BT0)])
          (_, []) ->
            case Staged.validatePrimBaseType tyName of
              Just _tyPrimBase -> pure ([], [], [])
              Nothing -> analysisError $ UnknownTypeOrInvalidArgs spanInFile tyName args
          ("List", [TypeArg tye]) -> do
            (tyeElem, bity@(BIType btElem _), cs) <- extractConstraintsFromTypeExpr btenv tye
            pure ([TypeArg tyeElem], [bity], cs ++ [CLeq ann bt btElem])
          ("Vec", [ExprArg e]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType btenv bt ann [(e, bityNat)]
            pure (map ExprArg exprArgs, [], cs)
          ("Mat", [ExprArg e1, ExprArg e2]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType btenv bt ann [(e1, bityNat), (e2, bityNat)]
            pure (map ExprArg exprArgs, [], cs)
          ("Tensor", [ExprArg eList]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType btenv bt ann [(eList, bityNatList)]
            pure (map ExprArg exprArgs, [], cs)
          (_, _) ->
            analysisError $ UnknownTypeOrInvalidArgs spanInFile tyName args
      let tye' = TypeExpr (bt, ann) (TyName tyName args')
      pure (tye', BIType bt (BITyBase bityBaseArgs), constraints)
    TyArrow (x1opt, tye1) tye2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr btenv tye1
      case x1opt of
        Nothing -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr btenv tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow (Nothing, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
        Just x1 -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <-
            extractConstraintsFromTypeExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow (Just x1, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyOptArrow (x1, tye1) tye2 -> do
      (tye1', bity1, constraints1) <- extractConstraintsFromTypeExpr btenv tye1
      (tye2', bity2, constraints2) <-
        extractConstraintsFromTypeExpr (Map.insert x1 (EntryLocallyBound bt bity1) btenv) tye2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let tye' = TypeExpr (bt, ann) (TyOptArrow (x1, tye1') tye2')
      pure (tye', BIType bt (BITyOptArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyProduct tye1 tye2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr btenv tye1
      (tye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr btenv tye2
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let tye' = TypeExpr (bt, ann) (TyProduct tye1' tye2')
      pure (tye', BIType bt (BITyProduct bity1 bity2), constraints1 ++ constraints2 ++ constraints)
  where
    bityNat :: BIType
    bityNat = BIType (BTConst BT0) (BITyBase [])

    bityNatList :: BIType
    bityNatList = BIType (BTConst BT0) (BITyBase [bityNat])

run :: SourceSpec -> BindingTimeEnv -> Expr -> Either AnalysisError (BExpr, [Constraint Span])
run sourceSpec btenv e = do
  let be = evalState (assignBindingTimeVarToExpr e) initialState
  (be', _bity, constraints) <- runReaderT (extractConstraintsFromExpr btenv be) analysisConfig
  pure (be', constraints)
  where
    analysisConfig = AnalysisConfig {sourceSpec}
