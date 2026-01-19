module Surface.BindingTime.Analyzer
  ( AnalysisError (..),
    run,
  )
where

import Control.Monad
import Data.Either.Extra (mapLeft)
import Data.Function ((&))
import Data.Map qualified as Map
import Safe.Exact (zipExactMay)
import Staged.Syntax qualified as Staged
import Surface.BindingTime.AnalysisError
import Surface.BindingTime.Constraint
import Surface.BindingTime.Core
import Surface.Syntax
import Util.Elaborator hiding (run)
import Util.Elaborator qualified as Elaborator
import Util.LocationInFile (SourceSpec, SpanInFile, getSpanInFile)
import Util.TokenUtil
import Prelude hiding (succ)

newtype AnalysisState = AnalysisState
  { nextBindingTimeVarIndex :: Int
  }

newtype AnalysisConfig = AnalysisConfig
  { sourceSpec :: SourceSpec
  }

type M trav a = Elaborator AnalysisState AnalysisConfig AnalysisError trav a

initialState :: AnalysisState
initialState =
  AnalysisState
    { nextBindingTimeVarIndex = 0
    }

fresh :: M trav BindingTimeVar
fresh = do
  AnalysisState {nextBindingTimeVarIndex = i} <- getState
  putState $ AnalysisState {nextBindingTimeVarIndex = i + 1}
  pure $ BindingTimeVar i

makeLam :: [LamBinder] -> Expr -> Expr
makeLam params eBody = do
  foldr go eBody params
  where
    go :: LamBinder -> Expr -> Expr
    go (MandatoryBinder labelOpt (x, ty@(TypeExpr loc1 _))) e@(Expr loc2 _) =
      -- TODO (enhance): give better range:
      Expr (mergeSpan loc1 loc2) (Lam Nothing labelOpt (x, ty) e)
    go (ImplicitBinder (x, ty@(TypeExpr loc1 _))) e@(Expr loc2 _) =
      -- TODO (enhance): give better range:
      Expr (mergeSpan loc1 loc2) (LamOpt (x, ty) e)

makeRecLam :: trav -> Var -> [LamBinder] -> TypeExpr -> Expr -> M trav Expr
makeRecLam _trav f params tyBody eBody = do
  (x0, ty0, paramsRest) <-
    case params of
      MandatoryBinder Nothing (x0', ty0') : paramsRest' -> pure (x0', ty0', paramsRest')
      MandatoryBinder (Just _) _ : _ -> error "TODO (error): makeLamRec, with a label"
      ImplicitBinder _ : _ -> error "TODO (error): makeLamRec, implicit binder"
      [] -> error "TODO (error): makeLamRec, empty parameter sequence"
  let (eRest, tyRest) = foldr go (eBody, tyBody) paramsRest
  let ann =
        -- TODO (enhance): give better code position
        let TypeExpr loc1 _ = ty0
            Expr loc2 _ = eBody
         in mergeSpan loc1 loc2
  let tyRec = TypeExpr ann (TyArrow Nothing (Just x0, ty0) tyRest)
  pure $ Expr ann (Lam (Just (f, tyRec)) Nothing (x0, ty0) eRest)
  where
    go :: LamBinder -> (Expr, TypeExpr) -> (Expr, TypeExpr)
    go (MandatoryBinder labelOpt (x, ty@(TypeExpr loc1 _))) (eAcc@(Expr loc2 _), tyAcc) = do
      let ann = mergeSpan loc1 loc2 -- TODO (enhance): give better code position
      let eAcc' = Expr ann (Lam Nothing labelOpt (x, ty) eAcc)
      let tyAcc' = TypeExpr ann (TyArrow labelOpt (Just x, ty) tyAcc)
      (eAcc', tyAcc')
    go (ImplicitBinder (x, ty@(TypeExpr loc1 _))) (eAcc@(Expr loc2 _), tyAcc) = do
      let ann = mergeSpan loc1 loc2 -- TODO (enhance): give better code position
      let eAcc' = Expr ann (LamOpt (x, ty) eAcc)
      let tyAcc' = TypeExpr ann (TyOptArrow (x, ty) tyAcc)
      (eAcc', tyAcc')

analysisError :: trav -> AnalysisError -> M trav a
analysisError = raiseError

askSpanInFile :: Span -> M trav SpanInFile
askSpanInFile loc = do
  AnalysisConfig {sourceSpec} <- askConfig
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
        let bity = error "TODO: BIType variable (not BindingTimeVar)"
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

openModule :: trav -> SpanInFile -> Var -> BindingTimeEnv -> M trav BindingTimeEnv
openModule trav spanInFile m btenv =
  case Map.lookup m btenv of
    Just (EntryModule btenv') ->
      pure $ Map.union btenv' btenv
    _ ->
      analysisError trav $ NotAModule spanInFile m

extractConstraintsFromExpr :: trav -> BindingTimeEnv -> Expr -> M trav (BExpr, BIType, [Constraint Span])
extractConstraintsFromExpr trav btenv (Expr ann exprMain) = do
  btv <- fresh
  let bt = BTVar btv
  spanInFile <- askSpanInFile ann
  case exprMain of
    Literal lit -> do
      (lit', bityBaseArgs, constraints) <- extractConstraintsFromLiteral trav btenv (bt, ann) lit
      pure (Expr (bt, ann) (Literal lit'), BIType bt (BITyBase bityBaseArgs), constraints)
    Var (ms, x) -> do
      (x', bity, constraints) <-
        case findVal btenv ms x of
          Nothing ->
            analysisError trav $ UnboundVar spanInFile ms x
          Just (EntryBuiltInPersistent x' bityVoid) ->
            pure (x', enhanceBIType (\() -> bt) bityVoid, [])
          Just (EntryBuiltInFixed x' btc' bityConst) ->
            pure (x', enhanceBIType BTConst bityConst, [CEqual ann bt (BTConst btc')])
          Just (EntryLocallyBound bt' bity) ->
            pure (x, bity, [CEqual ann bt bt'])
          Just (EntryModule _) ->
            analysisError trav $ NotAVal spanInFile ms x
      pure (Expr (bt, ann) (Var (ms, x')), bity, constraints)
    Lam Nothing labelOpt (x1, btye1) e2 -> do
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr trav (Map.insert x1 (EntryLocallyBound bt bity1) btenv) e2
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = Expr (bt, ann) (Lam Nothing labelOpt (x1, btye1') e2')
      pure (e', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    Lam (Just (f, btyeRec)) labelOpt (x1, btye1) e2 -> do
      -- Not confident. TODO: check the validity of the following
      (btyeRec', bityRec, constraintsRec) <- extractConstraintsFromTypeExpr trav btenv btyeRec
      (btye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv btye1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr
          trav
          (Map.insert x1 (EntryLocallyBound bt bity1) (Map.insert f (EntryLocallyBound bt bityRec) btenv))
          e2
      let bitySynth = BIType bt (BITyArrow bity1 bity2)
      constraintsEq <- makeConstraintsFromBITypeEquation trav ann bitySynth bityRec
      let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
      let e' = Expr (bt, ann) (Lam (Just (f, btyeRec')) labelOpt (x1, btye1') e2')
      pure (e', bitySynth, constraintsRec ++ constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
    App e1 labelOpt e2 -> do
      (e1WithoutOpts, bity1WithoutOpts, constraints1) <- extractConstraintsFromExpr trav btenv e1
      let (e1', bity1@(BIType bt1 bityMain1)) = appendOmittedOptionalArguments e1WithoutOpts bity1WithoutOpts
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
      pure (Expr (bt, ann) (App e1' labelOpt e2'), bity, constraints)
    LetIn x params eBody e2 -> do
      let e1 = makeLam params eBody
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr trav (Map.insert x (EntryLocallyBound bt bity1) btenv) e2
      let e' = Expr (bt, ann) (LetIn x [] e1' e2')
      pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    LetRecIn x params tye eBody e2 -> do
      e1 <- makeRecLam trav x params tye eBody
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <-
        extractConstraintsFromExpr trav (Map.insert x (EntryLocallyBound bt bity1) btenv) e2
      let e' = Expr (bt, ann) (LetIn x [] e1' e2')
      pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    LetTupleIn xL xR e1 e2 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr trav btenv e1
      case bityMain1 of
        BITyProduct bityL@(BIType btL _) bityR@(BIType btR _) -> do
          (e2', bity2@(BIType bt2 _), constraints2) <-
            extractConstraintsFromExpr
              trav
              (btenv & Map.insert xL (EntryLocallyBound btL bityL) & Map.insert xR (EntryLocallyBound btR bityR))
              e2
          let e' = Expr (bt, ann) (LetTupleIn xL xR e1' e2')
          pure (e', bity2, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
        _ -> do
          let Expr ann1 _ = e1
          spanInFile1 <- askSpanInFile ann1
          analysisError trav $ NotATuple spanInFile1 bity1
    LetOpenIn m e1 -> do
      (e1', bity1@(BIType bt1 _), constraints) <- do
        btenv' <- openModule trav spanInFile m btenv
        extractConstraintsFromExpr trav btenv' e1
      pure (Expr (bt, ann) (LetOpenIn m e1'), bity1, constraints ++ [CEqual ann bt bt1])
    Sequential e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromExpr trav btenv e2
      case bityMain1 of
        BITyBase [] -> do
          let e' = Expr (bt, ann) (Sequential e1' e2')
          pure (e', bity2, constraints1 ++ constraints2 ++ [CEqual ann bt bt1, CLeq ann bt bt2])
        _ -> do
          let Expr ann1 _ = e1
          spanInFile1 <- askSpanInFile ann1
          analysisError trav $ NotABase spanInFile1 bity1
    Tuple e1 e2 -> do
      -- Not confident. TODO: check the validity of the following
      (e1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromExpr trav btenv e2
      let e' = Expr (bt, ann) (Tuple e1' e2')
      let bity = BIType bt (BITyProduct bity1 bity2)
      pure (e', bity, constraints1 ++ constraints2 ++ [CLeq ann bt bt1, CLeq ann bt bt2])
    IfThenElse e0 e1 e2 -> do
      (e0', bity0@(BIType bt0 bityMain0), constraints0) <- extractConstraintsFromExpr trav btenv e0
      case bityMain0 of
        BITyBase [] -> do
          (e1', bity1, constraints1) <- extractConstraintsFromExpr trav btenv e1
          (e2', bity2, constraints2) <- extractConstraintsFromExpr trav btenv e2
          let e' = Expr (bt, ann) (IfThenElse e0' e1' e2')
          constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity1 bity2
          pure (e', bity1, constraints0 ++ constraints1 ++ constraints2 ++ constraintsEq ++ [CEqual ann bt bt0])
        _ -> do
          let Expr ann0 _ = e0
          spanInFile0 <- askSpanInFile ann0
          analysisError trav $ NotABase spanInFile0 bity0
    As e1 btye2 -> do
      (e1', bity1, constraints1) <- extractConstraintsFromExpr trav btenv e1
      (btye2', bity2, constraints2) <- extractConstraintsFromTypeExpr trav btenv btye2
      constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity1 bity2
      pure (Expr (bt, ann) (As e1' btye2'), bity2, constraints1 ++ constraints2 ++ constraintsEq)
    LamOpt (x1, btye1) e2 -> do
      (btye1', bity1, constraints1) <- extractConstraintsFromTypeExpr trav btenv btye1
      (e2', bity2, constraints2) <-
        extractConstraintsFromExpr trav (Map.insert x1 (EntryLocallyBound bt bity1) btenv) e2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let e' = Expr (bt, ann) (LamOpt (x1, btye1') e2')
      pure (e', BIType bt (BITyOptArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    AppOptGiven e1 e2 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (e2', bity2, constraints2) <- extractConstraintsFromExpr trav btenv e2
      (bity, constraints) <-
        case bityMain1 of
          BITyOptArrow bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            constraintsEq <- makeConstraintsFromBITypeEquation trav ann bity2 bity11
            pure (bity12, constraints1 ++ constraints2 ++ constraintsEq ++ constraints)
          _ -> do
            let Expr ann1 _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError trav $ NotAnOptFunction spanInFile1 bity1
      pure (Expr (bt, ann) (AppOptGiven e1' e2'), bity, constraints)
    AppOptOmitted e1 -> do
      (e1', bity1@(BIType bt1 bityMain1), constraints1) <- extractConstraintsFromExpr trav btenv e1
      (bity, constraints) <-
        case bityMain1 of
          BITyOptArrow _bity11 bity12 -> do
            let constraints = [CEqual ann bt bt1]
            pure (bity12, constraints1 ++ constraints)
          _ -> do
            let Expr ann1 _ = e1
            spanInFile1 <- askSpanInFile ann1
            analysisError trav $ NotAnOptFunction spanInFile1 bity1
      pure (Expr (bt, ann) (AppOptOmitted e1'), bity, constraints)

appendOmittedOptionalArguments :: BExpr -> BIType -> (BExpr, BIType)
appendOmittedOptionalArguments e@(Expr (_, ann) _) bity@(BIType _bt bityMain) =
  case bityMain of
    BITyOptArrow _bity1 bity2 ->
      -- TODO (enhance): give better location than `ann`
      appendOmittedOptionalArguments (Expr (BTConst BT0, ann) (AppOptOmitted e)) bity2
    _ ->
      (e, bity)

makeConstraintsFromBITypeEquation :: forall trav. trav -> Span -> BIType -> BIType -> M trav [Constraint Span]
makeConstraintsFromBITypeEquation trav ann bity1' bity2' = go bity1' bity2'
  where
    go :: BIType -> BIType -> M trav [Constraint Span]
    go bity1@(BIType bt1 bityMain1) bity2@(BIType bt2 bityMain2) =
      ([CEqual ann bt1 bt2] ++)
        <$> case (bityMain1, bityMain2) of
          (BITyBase bityBaseArgs1, BITyBase bityBaseArgs2) ->
            case zipExactMay bityBaseArgs1 bityBaseArgs2 of
              Nothing -> do
                spanInFile <- askSpanInFile ann
                analysisError trav $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2
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
            analysisError trav $ BITypeContradiction spanInFile bity1' bity2' bity1 bity2

extractConstraintsFromExprArgsForType :: trav -> BindingTimeEnv -> BindingTime -> Span -> [(Expr, BIType)] -> M trav ([BExpr], [Constraint Span])
extractConstraintsFromExprArgsForType trav btenv bt ann argsWithBityReq = do
  pairs <-
    mapM
      ( \(e, bityReq) -> do
          (e', bityGot, constraints') <- extractConstraintsFromExpr trav btenv e
          constraintsEq <- makeConstraintsFromBITypeEquation trav ann bityGot bityReq
          pure (e', constraints' ++ constraintsEq)
      )
      argsWithBityReq
  let args' = map fst pairs
  let constraints' = concatMap snd pairs
  let constraints = constraints' ++ [CEqual ann bt (BTConst BT1)]
  pure (args', constraints)

extractConstraintsFromTypeExpr :: trav -> BindingTimeEnv -> TypeExpr -> M trav (BTypeExpr, BIType, [Constraint Span])
extractConstraintsFromTypeExpr trav btenv (TypeExpr ann typeExprMain) = do
  btv <- fresh
  let bt = BTVar btv
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
              Nothing -> analysisError trav $ UnknownTypeOrInvalidArgs spanInFile tyName args
          ("List", [TypeArg tye]) -> do
            (tyeElem, bity@(BIType btElem _), cs) <- extractConstraintsFromTypeExpr trav btenv tye
            pure ([TypeArg tyeElem], [bity], cs ++ [CLeq ann bt btElem])
          ("Vec", [ExprArg e]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType trav btenv bt ann [(e, bityNat)]
            pure (map ExprArg exprArgs, [], cs)
          ("Mat", [ExprArg e1, ExprArg e2]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType trav btenv bt ann [(e1, bityNat), (e2, bityNat)]
            pure (map ExprArg exprArgs, [], cs)
          ("Tensor", [ExprArg eList]) -> do
            (exprArgs, cs) <- extractConstraintsFromExprArgsForType trav btenv bt ann [(eList, bityNatList)]
            pure (map ExprArg exprArgs, [], cs)
          ("Dataset", [ExprArg e1, ExprArg e2, ExprArg e3, ExprArg e4]) -> do
            (exprArgs, cs) <-
              extractConstraintsFromExprArgsForType trav btenv bt ann $
                [(e1, bityNatList), (e2, bityNatList), (e3, bityNat), (e4, bityNat)]
            pure (map ExprArg exprArgs, [], cs)
          (_, _) ->
            analysisError trav $ UnknownTypeOrInvalidArgs spanInFile tyName args
      let tye' = TypeExpr (bt, ann) (TyName tyName args')
      pure (tye', BIType bt (BITyBase bityBaseArgs), constraints)
    TyArrow labelOpt (x1opt, tye1) tye2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      case x1opt of
        Nothing -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr trav btenv tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow labelOpt (Nothing, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
        Just x1 -> do
          (tye2', bity2@(BIType bt2 _), constraints2) <-
            extractConstraintsFromTypeExpr trav (Map.insert x1 (EntryLocallyBound bt bity1) btenv) tye2
          let constraints = [CLeq ann bt bt1, CLeq ann bt bt2]
          let tye' = TypeExpr (bt, ann) (TyArrow labelOpt (Just x1, tye1') tye2')
          pure (tye', BIType bt (BITyArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyOptArrow (x1, tye1) tye2 -> do
      (tye1', bity1, constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      (tye2', bity2, constraints2) <-
        extractConstraintsFromTypeExpr trav (Map.insert x1 (EntryLocallyBound bt bity1) btenv) tye2
      let constraints = [CEqual ann bt (BTConst BT0)]
      let tye' = TypeExpr (bt, ann) (TyOptArrow (x1, tye1') tye2')
      pure (tye', BIType bt (BITyOptArrow bity1 bity2), constraints1 ++ constraints2 ++ constraints)
    TyProduct tye1 tye2 -> do
      (tye1', bity1@(BIType bt1 _), constraints1) <- extractConstraintsFromTypeExpr trav btenv tye1
      (tye2', bity2@(BIType bt2 _), constraints2) <- extractConstraintsFromTypeExpr trav btenv tye2
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
  let (result, _finalState) = Elaborator.run (extractConstraintsFromExpr () btenv e) analysisConfig initialState
  (be', _bity, constraints) <- mapLeft fst result
  pure (be', constraints)
  where
    analysisConfig = AnalysisConfig {sourceSpec}
