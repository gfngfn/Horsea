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
import Data.List (length)
import Data.List.Extra (firstJust)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tensor.Matrix qualified as Matrix
import Data.Tensor.Vector qualified as Vector
import Data.Text (Text)
import Data.Tuple.Extra (both, first, second)
import Safe.Exact (zipExactMay)
import Staged.BuiltIn qualified as BuiltIn
import Staged.BuiltIn.Core
import Staged.Core
import Staged.SrcSyntax
import Staged.Subst
import Staged.Syntax
import Staged.TypeError
import Staged.TypeSubst
import Staged.Typechecker.CastInsertion
import Staged.Typechecker.Instantiation
import Staged.Typechecker.Merging
import Staged.Typechecker.Monad
import Staged.Typechecker.SigRecord (Ass0Metadata (..), Ass1Metadata (..), Ass1TypeParam (..), AssPersMetadata (..), ModuleEntry (..), SigRecord, TypeEntry (..), ValEntry (..))
import Staged.Typechecker.SigRecord qualified as SigRecord
import Staged.Typechecker.TypeEnv (TypeEnv, TypeVarEntry (..))
import Staged.Typechecker.TypeEnv qualified as TypeEnv
import Prelude hiding (length)

bug :: String -> a
bug msg = error $ "bug: " ++ msg

findValVar :: trav -> Span -> [ModuleName] -> Var -> TypeEnv -> M trav ValEntry
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
    go :: SigRecord -> [ModuleName] -> Maybe ValEntry
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
          (cast, _varSolution, _tyvar0Solution) <-
            makeAssertiveCast trav loc (TypeEnv.datatypeOnly tyEnv) Set.empty Set.empty a0tye a0tyeReq
          pure $ applyCast0 cast a0e
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
      (cast, _varSolution, _tyvar0Solution) <-
        makeAssertiveCast trav loc (TypeEnv.datatypeOnly tyEnv) Set.empty Set.empty a0tye a0tyeReq
      pure $ applyCast0 cast a0e

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
                typeError trav $ InvalidConstructorApplication spanInFile appCtx mods ctor
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
        result <- instantiateGuidedByAppContext0 trav loc (TypeEnv.datatypeOnly tyEnv) appCtx a0tye
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
                  makeAssertiveCast
                    trav
                    loc
                    (TypeEnv.datatypeOnly tyEnv)
                    Set.empty
                    Set.empty
                    a0tyeSynth
                    a0tyeRec
                let sa0tyeRec = strictify a0tyeRec
                let sa0tye1 = strictify a0tye1
                pure (Pure a0tyeRec, applyCast0 cast (A0Lam (Just (af, sa0tyeRec)) (ax1, sa0tye1) a0e2))
          _ : _ ->
            -- TODO (enhance): consider supporting lambda abstractions with direct arguments
            typeError trav $ Unsupported spanInFile $ LamWithArguments appCtx
      App e1 labelOpt e2 -> do
        (a0tye2, a0e2) <- typecheckExpr0Single trav tyEnv e2
        (result1, a0e1) <- typecheckExpr0 trav tyEnv (AppArg0 labelOpt a0e2 a0tye2 : appCtx) e1
        case result1 of
          Cast0 cast _a0tye11 result -> do
            pure (result, A0App a0e1 (applyCast0 cast a0e2))
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
            pure (result, A0App a0e1 (A0Constructor "Just" [applyCast0 cast a0e2]))
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
            pure (result, A0App a0e1 (applyCast0 cast a0e2))
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
          makeAssertiveCast trav loc (TypeEnv.datatypeOnly tyEnv) Set.empty Set.empty a0tye1Synth a0tye1Rec
        let a0e1 = applyCast0 cast (A0Lam (Just (afInner, strictify a0tye1Rec)) (ax0, strictify a0tyeParam0) a0eRest)
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
      Record fields -> do
        case appCtx of
          [] -> do
            (a0rty, a0re) <-
              foldM
                ( \(a0rty', a0re') (label, field) ->
                    if Map.member label a0rty'
                      then
                        typeError trav $ DuplicateRecordField spanInFile label
                      else case field of
                        RecordFieldEqual e -> do
                          (a0tye, a0e) <- typecheckExpr0Single trav tyEnv e
                          pure (Map.insert label a0tye a0rty', Map.insert label a0e a0re')
                        RecordFieldColon _ ->
                          typeError trav $ InvalidSyntaxAsExpr spanInFile
                )
                (Map.empty, Map.empty)
                fields
            pure (Pure (A0TyRecord a0rty), A0Record a0re)
          _ : _ -> do
            typeError trav $ CannotApplyRecord spanInFile
      FieldProj e1 label -> do
        (a0tye1, a0e1) <- typecheckExpr0Single trav tyEnv e1
        case a0tye1 of
          A0TyRecord a0rty1 ->
            case Map.lookup label a0rty1 of
              Just a0tyeSub -> do
                result <- instantiateGuidedByAppContext0 trav loc (TypeEnv.datatypeOnly tyEnv) appCtx a0tyeSub
                pure (result, A0FieldProj a0e1 label)
              Nothing ->
                typeError trav $ NoRecordFieldAtStage0 spanInFile label a0rty1
          _ ->
            typeError trav $ NotARecordAtStage0 spanInFile a0tye1
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
          (eq, _varSolution, _tyvar1Solution) <-
            makeEquation1 trav loc (TypeEnv.datatypeOnly tyEnv) Set.empty Set.empty a1tye a1tyeReq
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
      (eq, _varSolution, _tyvar1Solution) <-
        makeEquation1 trav loc (TypeEnv.datatypeOnly tyEnv) Set.empty Set.empty a1tye a1tyeReq
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
                typeError trav $ InvalidConstructorApplication spanInFile appCtx mods ctor
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
                                makeEquation1
                                  trav
                                  locElem
                                  (TypeEnv.datatypeOnly tyEnv)
                                  Set.empty
                                  Set.empty
                                  a1tye
                                  a1tyeFirst
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
        (result, _) <- instantiateGuidedByAppContext1 trav loc (TypeEnv.datatypeOnly tyEnv) Set.empty appCtx a1tye
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
                  makeEquation1
                    trav
                    loc
                    (TypeEnv.datatypeOnly tyEnv)
                    Set.empty
                    Set.empty
                    a1tyeSynth
                    a1tyeRec
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
        (eq, _varSolution, _tyvar1Solution) <-
          makeEquation1
            trav
            loc
            (TypeEnv.datatypeOnly tyEnv)
            Set.empty
            Set.empty
            a1tye1Synth
            a1tye1Rec
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
      Record fields -> do
        case appCtx of
          [] -> do
            (a1rty, a1re) <-
              foldM
                ( \(a1rty', a1re') (label, field) ->
                    if Map.member label a1rty'
                      then
                        typeError trav $ DuplicateRecordField spanInFile label
                      else case field of
                        RecordFieldEqual e -> do
                          (a1tye, a1e) <- typecheckExpr1Single trav tyEnv e
                          pure (Map.insert label a1tye a1rty', Map.insert label a1e a1re')
                        RecordFieldColon _ ->
                          typeError trav $ InvalidSyntaxAsExpr spanInFile
                )
                (Map.empty, Map.empty)
                fields
            pure (Pure (A1TyRecord a1rty), A1Record a1re)
          _ : _ -> do
            typeError trav $ CannotApplyRecord spanInFile
      FieldProj e1 label -> do
        (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
        case a1tye1 of
          A1TyRecord a1rty1 ->
            case Map.lookup label a1rty1 of
              Just a1tyeSub -> do
                (result, _) <-
                  instantiateGuidedByAppContext1
                    trav
                    loc
                    (TypeEnv.datatypeOnly tyEnv)
                    Set.empty
                    appCtx
                    a1tyeSub
                pure (result, A1FieldProj a1e1 label)
              Nothing ->
                typeError trav $ NoRecordFieldAtStage1 spanInFile label a1rty1
          _ ->
            typeError trav $ NotARecordAtStage1 spanInFile a1tye1
      IfThenElse e0 e1 e2 -> do
        (a1tye0, a1e0) <- typecheckExpr1Single trav tyEnv e0
        case a1tye0 of
          A1TyPrim (A1TyPrimBase ATyPrimBool) ->
            case appCtx of
              [] -> do
                (a1tye1, a1e1) <- typecheckExpr1Single trav tyEnv e1
                (a1tye2, a1e2) <- typecheckExpr1Single trav tyEnv e2
                (eq, _varSolution, _tyvar1Solution) <-
                  makeEquation1
                    trav
                    loc
                    (TypeEnv.datatypeOnly tyEnv)
                    Set.empty
                    Set.empty
                    a1tye2
                    a1tye1
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
    App {} -> do
      ((mods, tyName), args) <- collectTypeArgs trav loc tyeMain
      case (mods, tyName, args) of
        ([], "List", [arg1]) -> do
          a0tye1 <- typecheckTypeExpr0 trav tyEnv arg1
          pure $ A0TyList a0tye1 Nothing
        ([], "Maybe", [arg1]) -> do
          a0tye1 <- typecheckTypeExpr0 trav tyEnv arg1
          pure $ A0TyMaybe a0tye1
        ([], "Vec", [arg1@(Expr loc1 _)]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg1
          n1 <- validateIntLiteral trav loc1 a0e1
          pure $ A0TyPrim (a0TyVec n1) Nothing
        ([], "Mat", [arg1@(Expr loc1 _), arg2@(Expr loc2 _)]) -> do
          a0e1 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg1
          a0e2 <- forceExpr0 trav tyEnv BuiltIn.tyNat arg2
          n1 <- validateIntLiteral trav loc1 a0e1
          n2 <- validateIntLiteral trav loc2 a0e2
          pure $ A0TyPrim (a0TyMat n1 n2) Nothing
        ([], "Tensor", [arg@(Expr loc' _)]) -> do
          a0e <- forceExpr0 trav tyEnv (A0TyList BuiltIn.tyNat Nothing) arg
          ns <- validateIntListLiteral trav loc' a0e
          pure $ A0TyPrim (A0TyTensor ns) Nothing
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
                    A0App (A0App BuiltIn.ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
            A0TyList a0tyeElem Nothing -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) a0e2
            A0TyList a0tyeElem (Just a0ePredForBase) -> do
              pure $
                A0TyList a0tyeElem . Just $
                  A0Lam Nothing (ax, strictify a0tye1) $
                    A0App (A0App BuiltIn.ass0exprAnd (A0App a0ePredForBase (A0Var ax))) a0e2
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
    Record fields -> do
      a0rty <-
        foldM
          ( \a0rty' (label, field) ->
              if Map.member label a0rty'
                then
                  typeError trav $ DuplicateRecordField spanInFile label
                else case field of
                  RecordFieldEqual _ ->
                    typeError trav $ InvalidSyntaxAsTypeExpr spanInFile
                  RecordFieldColon tye -> do
                    a0tye <- typecheckTypeExpr0 trav tyEnv tye
                    pure $ Map.insert label a0tye a0rty'
          )
          Map.empty
          fields
      pure $ A0TyRecord a0rty
    TyForAll tyvar tye1 -> do
      atyvar <- generateFreshTypeVar tyvar
      a0tye1 <- do
        let tyEnv' = TypeEnv.addTypeVar tyvar (TypeVarEntry0 atyvar) tyEnv
        typecheckTypeExpr0 trav tyEnv' tye1
      pure $ A0TyForAll atyvar a0tye1
    (Literal {}; Var {}; Lam {}; LetIn {}; LetRecIn {}; LetTupleIn {}; IfThenElse {}; Case {}; As {}; Escape _; LamOms {}; AppOms {}; LamInf {}; AppInfGiven {}; AppInfOmitted {}; LetOpenIn {}; Sequential {}; Tuple {}; FieldProj {}; LamInfType {}; AppInfType {}; Persistent {}) ->
      typeError trav $ InvalidSyntaxAsTypeExpr spanInFile

validatePersistentExprArg :: trav -> TypeEnv -> Ass0TypeExpr -> Expr -> M trav Ass0Expr
validatePersistentExprArg trav tyEnv tyReq (Expr loc eMain) =
  case eMain of
    Persistent e ->
      forceExpr0 trav tyEnv tyReq e
    _ -> do
      spanInFile <- askSpanInFile loc
      typeError trav $ CannotUseNormalArgAtStage1 spanInFile

findType :: trav -> Span -> [ModuleName] -> TypeName -> TypeEnv -> M trav (Maybe TypeEntry)
findType trav loc mods tyName tyEnv =
  case mods of
    [] ->
      pure $ TypeEnv.findType tyName tyEnv
    modName : modsRest ->
      case TypeEnv.findModule modName tyEnv of
        Nothing -> do
          spanInFile <- askSpanInFile loc
          typeError trav $ UnboundModule spanInFile modName
        Just (ModuleEntry sigr) -> do
          sigr' <- go sigr modsRest
          pure $ SigRecord.findType tyName sigr'
  where
    go sigr = \case
      [] ->
        pure sigr
      modName : modsRest ->
        case SigRecord.findModule modName sigr of
          Nothing -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ UnboundModule spanInFile modName
          Just (ModuleEntry sigr') ->
            go sigr' modsRest

typecheckTypeExpr1 :: trav -> TypeEnv -> TypeExpr -> M trav Ass1TypeExpr
typecheckTypeExpr1 trav tyEnv (Expr loc tyeMain) = do
  spanInFile <- askSpanInFile loc
  case tyeMain of
    Constructor (mods, tyName) -> do
      tyEntry_ <- findType trav loc mods tyName tyEnv
      case tyEntry_ of
        Just tyEntry -> do
          case tyEntry of
            Ass1TypeAlias a1tyParams a1tye ->
              case a1tyParams of
                [] -> pure a1tye
                _ : _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile [] tyName (length a1tyParams)
            Ass1TypeData a1tyParams datatyId ->
              case a1tyParams of
                [] -> pure $ A1TyData datatyId []
                _ : _ -> typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile [] tyName (length a1tyParams)
        Nothing ->
          case mods of
            [] ->
              case validatePrimBaseType tyName of
                Just tyPrimBase -> pure $ A1TyPrim (A1TyPrimBase tyPrimBase)
                Nothing -> typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile mods tyName 0
            _ : _ ->
              typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile mods tyName 0
    App {} -> do
      ((mods, tyName), args) <- collectTypeArgs trav loc tyeMain
      tyEntry_ <- findType trav loc mods tyName tyEnv
      case tyEntry_ of
        Just tyEntry -> do
          (a1tye, hasValArg) <-
            case tyEntry of
              Ass1TypeAlias a1tyParams a1tyeBody ->
                case zipExactMay a1tyParams args of
                  Just zipped -> do
                    foldM
                      ( \(a1tye', hasValArg') (a1tyParam, arg) ->
                          case a1tyParam of
                            A1TypeParamType atyvar -> do
                              a1tyeArg <- typecheckTypeExpr1 trav tyEnv arg
                              pure (tySubst1 a1tyeArg atyvar a1tye', hasValArg')
                            A1TypeParamVal0 ax a0tye -> do
                              a0eArg <- validatePersistentExprArg trav tyEnv a0tye arg
                              pure (subst0 a0eArg ax a1tye', True)
                      )
                      (a1tyeBody, False)
                      zipped
                  Nothing ->
                    typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile [] tyName (length a1tyParams)
              Ass1TypeData a1tyParams datatyId -> do
                case zipExactMay a1tyParams args of
                  Just zipped -> do
                    first (A1TyData datatyId . reverse)
                      <$> foldM
                        ( \(a1datatyArgAcc', hasValArg') (a1tyParam, arg) ->
                            case a1tyParam of
                              A1TypeParamType _atyvar -> do
                                -- TODO: use `atyvar`?
                                a1tyeArg <- typecheckTypeExpr1 trav tyEnv arg
                                pure (A1DatatypeArgType a1tyeArg : a1datatyArgAcc', hasValArg')
                              A1TypeParamVal0 _ax a0tye -> do
                                -- TODO: use `ax`?
                                a0eArg <- validatePersistentExprArg trav tyEnv a0tye arg
                                pure (A1DatatypeArgVal0 a0eArg : a1datatyArgAcc', True)
                        )
                        ([], False)
                        zipped
                  Nothing ->
                    typeError trav $ UnknownTypeOrInvalidArityAtStage0 spanInFile [] tyName (length a1tyParams)
          when hasValArg $ logShapeAnnot (ShapeAnnotLog loc)
          pure a1tye
        Nothing ->
          case mods of
            [] ->
              case (tyName, args) of
                ("List", [tye]) -> do
                  a1tye <- typecheckTypeExpr1 trav tyEnv tye
                  pure $ A1TyList a1tye
                ("Maybe", [tye]) -> do
                  a1tye <- typecheckTypeExpr1 trav tyEnv tye
                  pure $ A1TyMaybe a1tye
                ("Vec", [arg]) -> do
                  a0e <- validatePersistentExprArg trav tyEnv BuiltIn.tyNat arg
                  pure $ A1TyPrim (a1TyVec a0e)
                ("Mat", [arg1, arg2]) -> do
                  a0e1 <- validatePersistentExprArg trav tyEnv BuiltIn.tyNat arg1
                  a0e2 <- validatePersistentExprArg trav tyEnv BuiltIn.tyNat arg2
                  pure $ A1TyPrim (a1TyMat a0e1 a0e2)
                ("Tensor", [arg]) -> do
                  logShapeAnnot (ShapeAnnotLog loc)
                  a0eList <- validatePersistentExprArg trav tyEnv (A0TyList BuiltIn.tyNat Nothing) arg
                  pure $ A1TyPrim (A1TyTensor a0eList)
                _ ->
                  typeError trav $ UnknownTypeOrInvalidArityAtStage1 spanInFile mods tyName (length args)
            _ : _ ->
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
    Record fields -> do
      a1rty <-
        foldM
          ( \a1rty' (label, field) ->
              if Map.member label a1rty'
                then
                  typeError trav $ DuplicateRecordField spanInFile label
                else case field of
                  RecordFieldEqual _ ->
                    typeError trav $ InvalidSyntaxAsTypeExpr spanInFile
                  RecordFieldColon tye -> do
                    a1tye <- typecheckTypeExpr1 trav tyEnv tye
                    pure $ Map.insert label a1tye a1rty'
          )
          Map.empty
          fields
      pure $ A1TyRecord a1rty
    TyForAll tyvar tye1 -> do
      atyvar <- generateFreshTypeVar tyvar
      a1tye1 <- do
        let tyEnv' = TypeEnv.addTypeVar tyvar (TypeVarEntry1 atyvar) tyEnv
        typecheckTypeExpr1 trav tyEnv' tye1
      pure $ A1TyForAll atyvar a1tye1
    (Literal _; Var _; Lam {}; LetIn {}; LetRecIn {}; LetTupleIn {}; IfThenElse {}; Case {}; As {}; Escape _; LamOms {}; AppOms {}; LamInf {}; AppInfGiven {}; AppInfOmitted {}; LetOpenIn {}; Sequential {}; Tuple {}; FieldProj {}; LamInfType {}; AppInfType {}; Persistent {}) ->
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
      A0TyRecord a0rty ->
        APersTyRecord <$> mapM go a0rty
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
    BindType stage tyName tyParams tydef ->
      case stage of
        Stage1 -> do
          (a1tyParamAcc, tyEnv') <-
            foldM
              ( \(a1tyParamAcc0, tyEnv0) tyParam ->
                  case tyParam of
                    TypeParamTypeBinder tyvar -> do
                      atyvar <- generateFreshTypeVar tyvar
                      let tyEnv1 = TypeEnv.addTypeVar tyvar (TypeVarEntry1 atyvar) tyEnv0
                      pure (A1TypeParamType atyvar : a1tyParamAcc0, tyEnv1)
                    TypeParamVal0Binder (x, tyeParam) -> do
                      svX <- generateFreshVar (Just x)
                      let ax = AssVarStatic svX
                      a0tyeParam <- typecheckTypeExpr0 trav tyEnv0 tyeParam
                      let tyEnv1 = TypeEnv.addVal x (Ass0Entry a0tyeParam (Right svX)) tyEnv0
                      pure (A1TypeParamVal0 ax a0tyeParam : a1tyParamAcc0, tyEnv1)
              )
              ([], tyEnv)
              tyParams
          let a1tyParams = reverse a1tyParamAcc
          case tydef of
            TypeDefAlias tyeBody -> do
              a1tyeBody <- typecheckTypeExpr1 trav tyEnv' tyeBody
              pure (SigRecord.singletonTypeAlias tyName a1tyParams a1tyeBody, [])
            TypeDefData ctorDefs -> do
              datatyId <- generateFreshDatatypeId tyName
              ctormap <-
                foldM
                  ( \ctormap' ((ctor, _), tye_) -> do
                      case tye_ of
                        Nothing ->
                          pure $ Map.insert ctor Nothing ctormap'
                        Just tye -> do
                          a1tye <- typecheckTypeExpr1 trav tyEnv' tye
                          pure $ Map.insert ctor (Just a1tye) ctormap'
                  )
                  Map.empty
                  ctorDefs
              pure (SigRecord.singletonTypeData tyName a1tyParams datatyId ctormap, [])
        _ ->
          error "TODO: BindType, non-Stage1"
    BindModule m binds -> do
      (_, sigr, abinds) <- typecheckBinds trav tyEnv binds
      pure (SigRecord.singletonModule m (ModuleEntry sigr), abinds)

typecheckBinds :: trav -> TypeEnv -> [Bind] -> M trav (TypeEnv, SigRecord, [AssBind])
typecheckBinds trav tyEnv =
  foldM
    ( \(tyEnv', sigr', abinds') bind@(Bind loc _) -> do
        (sigr, abinds) <- typecheckBind trav tyEnv' bind
        case SigRecord.intersection sigr' sigr of
          ([], [], []) ->
            pure (TypeEnv.appendSigRecord tyEnv' sigr, SigRecord.union sigr' sigr, abinds' ++ abinds)
          (x : _, _, _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile x
          (_, tyName : _, _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile tyName
          (_, _, m : _) -> do
            spanInFile <- askSpanInFile loc
            typeError trav $ BindingOverwritten spanInFile m
    )
    (tyEnv, SigRecord.empty, [])
