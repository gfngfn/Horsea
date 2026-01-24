{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.CompileTime
  ( BuiltInSpec (..),
    Common (..),
    BuiltInSpecMain (..),
    GenSpec (..),
    VersatileSpec (..),
    ParamSpec (..),
    deriveDecs,
    deriveDeltaReduction,
    deriveDisp,
  )
where

import Control.Monad.Extra (mapMaybeM)
import Data.List (foldl')
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (mapMaybe)
import Language.Haskell.TH qualified as TH
import Safe.Exact (zipExactMay)
import Prelude

data BuiltInSpec = BuiltInSpec
  { common :: Common,
    main :: BuiltInSpecMain
  }

data Common = Common
  { constructor0 :: String,
    constructorDisplay0 :: String
  }

data BuiltInSpecMain
  = Gen GenSpec
  | Versatile VersatileSpec

data GenSpec = GenSpec
  { params :: [ParamSpec],
    constructor1 :: String
  }

data VersatileSpec = VersatileSpec
  { fixedParams :: [ParamSpec],
    arity :: Int,
    bodyQ :: TH.Q TH.Exp
  }

data ParamSpec
  = ParamInt
  | ParamIntList

deriveDecs :: [BuiltInSpec] -> TH.Q [TH.Dec]
deriveDecs allBiSpecs = do
  let dec0s = map (deriveDecPerArity allBiSpecs) [1 .. 1]
  -- TODO: change "Foo" to "Ass1BuiltIn":
  let dec1 = TH.DataD [] (TH.mkName "Foo") [] Nothing (mapMaybe makeConstructor1 allBiSpecs) derivClauses
  let decs = dec1 : dec0s
  TH.runIO $ putStrLn $ "DECS: " ++ show decs
  pure decs
  where
    derivClauses :: [TH.DerivClause]
    derivClauses = [TH.DerivClause (Just TH.StockStrategy) [TH.ConT ''Eq, TH.ConT ''Show]]

    makeConstructor1 :: BuiltInSpec -> Maybe TH.Con
    makeConstructor1 BuiltInSpec {main} =
      case main of
        Gen GenSpec {constructor1, params} ->
          -- TODO: remove "Bar":
          pure $ TH.NormalC (TH.mkName ("Bar" ++ constructor1)) (map ((noBang,) . makeParam) params)
        Versatile _ ->
          Nothing

deriveDecPerArity :: [BuiltInSpec] -> Int -> TH.Dec
deriveDecPerArity allBiSpecs arity =
  TH.DataD [] builtInWithArityTypeName [] Nothing (map makeConstructor0 pairs) derivClauses
  where
    builtInWithArityTypeName = TH.mkName $ "BuiltInArity" ++ show arity

    pairs =
      mapMaybe
        ( \BuiltInSpec {common, main} ->
            case main of
              Gen genSpec ->
                if length genSpec.params == arity then Just (common, []) else Nothing
              Versatile versSpec ->
                if versSpec.arity == arity then Just (common, versSpec.fixedParams) else Nothing
        )
        allBiSpecs

    derivClauses :: [TH.DerivClause]
    derivClauses = [TH.DerivClause (Just TH.StockStrategy) [TH.ConT ''Eq, TH.ConT ''Show]]

    makeConstructor0 :: (Common, [ParamSpec]) -> TH.Con
    makeConstructor0 (common, fixedParams) =
      TH.NormalC (TH.mkName common.constructor0) (map ((noBang,) . makeParam) fixedParams)

noBang :: TH.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

makeParam :: ParamSpec -> TH.Type
makeParam = \case
  ParamInt -> TH.ConT ''Int
  ParamIntList -> TH.AppT (TH.ConT ''[]) (TH.ConT ''Int)

filterGen :: [BuiltInSpec] -> [(Common, GenSpec)]
filterGen =
  mapMaybe
    ( \BuiltInSpec {common, main} ->
        case main of
          Gen genSpec -> Just (common, genSpec)
          Versatile _ -> Nothing
    )

filterVersatile :: [BuiltInSpec] -> [(Common, VersatileSpec)]
filterVersatile =
  mapMaybe
    ( \BuiltInSpec {common, main} ->
        case main of
          Gen _ -> Nothing
          Versatile versSpec -> Just (common, versSpec)
    )

string :: String -> TH.Exp
string = TH.LitE . TH.StringL

-- | Generates the delta-reduction function
-- ```
-- reduceDeltaArity{n} :: BuiltInArity{n} -> Ass0Val -> ... -> Ass0Val -> M Ass0Val
--                                           \_________ n times ________/
-- ```
-- for each arity `n`.
deriveDeltaReduction :: [BuiltInSpec] -> TH.Q [TH.Dec]
deriveDeltaReduction allBiSpecs =
  concat <$> mapM (deriveDeltaReductionPerArity allBiSpecs) [1 .. 1]

deriveDeltaReductionPerArity :: [BuiltInSpec] -> Int -> TH.Q [TH.Dec]
deriveDeltaReductionPerArity allBiSpecs arity = do
  body <- makeBody
  pure
    [ TH.SigD reduceDeltaArityFunName (TH.ConT builtInWithArityTypeName `arr` funType arity),
      TH.FunD reduceDeltaArityFunName [TH.Clause (map TH.VarP (biName : argValNames)) (TH.NormalB body) []]
    ]
  where
    arr :: TH.Type -> TH.Type -> TH.Type
    arr ty1 = TH.AppT (TH.AppT TH.ArrowT ty1)

    funType n =
      if n <= 0
        then TH.AppT (TH.ConT (TH.mkName "M")) (TH.ConT ass0ValTypeName)
        else TH.ConT ass0ValTypeName `arr` funType (n - 1)

    builtInWithArityTypeName = TH.mkName $ "BuiltInArity" ++ show arity
    ass0ValTypeName = TH.mkName $ "Ass0Val"
    reduceDeltaArityFunName = TH.mkName $ "reduceDeltaArity" ++ show arity
    biName = TH.mkName $ "bi" ++ show arity
    namePairs = map (\i -> (TH.mkName ("x" ++ show i), TH.mkName ("a0v" ++ show i))) [1 .. arity]
    validatedValNames = map fst namePairs
    argValNames = map snd namePairs

    allGenPairs = filterGen allBiSpecs
    allVersPairs = filterVersatile allBiSpecs

    -- The body of the function `reduceDeltaArity{N}`.
    makeBody :: TH.Q TH.Exp
    makeBody = do
      TH.CaseE (TH.VarE biName) <$>
        ((mapMaybe makeGenBranch allGenPairs ++) <$> mapMaybeM makeVersatileBranch allVersPairs)

    -- Constructs a branch for handling one *Gen-based* built-in function.
    -- Returns `Nothing` for those of unwanted arities.
    makeGenBranch :: (Common, GenSpec) -> Maybe TH.Match
    makeGenBranch (common, genSpec) = do
      zipped <- zipExactMay genSpec.params namePairs
      let branchBody = TH.DoE Nothing (map makeStmt zipped ++ [TH.NoBindS (TH.AppE (TH.VarE 'pure) retVal)])
      pure $ TH.Match pat (TH.NormalB branchBody) []
      where
        pat :: TH.Pat
        pat = TH.ConP (TH.mkName common.constructor0) [] []

        makeStmt :: (ParamSpec, (TH.Name, TH.Name)) -> TH.Stmt
        makeStmt (paramSpec, (validatedValName, argValName)) =
          TH.BindS (TH.VarP validatedValName) (TH.AppE validator (TH.VarE argValName))
          where
            validator =
              TH.VarE $
                TH.mkName $
                  case paramSpec of
                    ParamInt -> "validateIntLiteral"
                    ParamIntList -> "validateIntListLiteral"

        retVal :: TH.Exp
        retVal =
          TH.AppE (TH.ConE (TH.mkName "A0ValBracket")) $
            TH.AppE (TH.ConE (TH.mkName "A1ValConst")) $
              foldl'
                TH.AppE
                (TH.ConE (TH.mkName genSpec.constructor1))
                (map TH.VarE validatedValNames)

    -- Constructs a branch for handling one *Versatile-based* built-in function.
    -- Returns `Nothing` for those of unwanted arities.
    -- Fixed parameters are handled by variable names `p1`, ..., `p{k}`.
    makeVersatileBranch :: (Common, VersatileSpec) -> TH.Q (Maybe TH.Match)
    makeVersatileBranch (common, versSpec) = do
      if versSpec.arity /= arity
        then
          pure Nothing
        else do
          branchBody <- versSpec.bodyQ
          pure . Just $ TH.Match pat (TH.NormalB branchBody) []
      where
        pat = TH.ConP (TH.mkName common.constructor0) [] (map TH.VarP fixedParamVars)
        fixedParamVars = map (\i -> TH.mkName ("p" ++ show i)) [1 .. length versSpec.fixedParams]

-- | Generates `instance Disp BuiltInArity{n} where ...` for each arity.
deriveDisp :: [BuiltInSpec] -> TH.Q [TH.Dec]
deriveDisp allBiSpecs =
  mapM (deriveDispPerArity allBiSpecs) [1 .. 1]

deriveDispPerArity :: [BuiltInSpec] -> Int -> TH.Q TH.Dec
deriveDispPerArity allBiSpecs arity =
  pure $
    TH.InstanceD
      Nothing
      []
      (TH.AppT (TH.ConT dispTypeClassName) (TH.ConT builtInWithArityTypeName))
      [ TH.FunD
          dispGenFunName
          [ TH.Clause [TH.WildP] (TH.NormalB (TH.LamCaseE branches)) []
          ]
      ]
  where
    dispTypeClassName = TH.mkName "Disp"
    dispGenFunName = TH.mkName "dispGen"
    builtInWithArityTypeName = TH.mkName $ "BuiltInArity" ++ show arity

    allGenPairs = filterGen allBiSpecs
    allVersPairs = filterVersatile allBiSpecs

    branches :: [TH.Match]
    branches = mapMaybe makeGenBranch allGenPairs ++ mapMaybe makeVersatileBranch allVersPairs

    makeGenBranch :: (Common, GenSpec) -> Maybe TH.Match
    makeGenBranch (common, genSpec) =
      if length genSpec.params /= arity
        then Nothing
        else pure $ TH.Match pat (TH.NormalB branchBody) []
      where
        pat :: TH.Pat
        pat = TH.ConP (TH.mkName common.constructor0) [] []

        branchBody :: TH.Exp
        branchBody = string common.constructorDisplay0

    makeVersatileBranch :: (Common, VersatileSpec) -> Maybe TH.Match
    makeVersatileBranch (common, versSpec) = do
      if versSpec.arity /= arity
        then Nothing
        else pure $ TH.Match pat (TH.NormalB branchBody) []
      where
        fixedParamVars :: [(TH.Name, ParamSpec)]
        fixedParamVars =
          map
            (\(i, paramSpec) -> (TH.mkName ("p" ++ show i), paramSpec))
            (zip [(1 :: Int) ..] versSpec.fixedParams)

        pat :: TH.Pat
        pat = TH.ConP (TH.mkName common.constructor0) [] (map (TH.VarP . fst) fixedParamVars)

        branchBody :: TH.Exp
        branchBody =
          case nonEmpty fixedParamVars of
            Nothing ->
              string common.constructorDisplay0
            Just fixedParamVarsNonEmpty ->
              joinDocsDirect
                (string common.constructorDisplay0)
                (joinDocsDirect (joinDocsDirect (string "@{") paramDispE) (string "}"))
              where
                paramDispE =
                  foldl1 joinDocsWithComma (fmap makeParamDisp fixedParamVarsNonEmpty)

        makeParamDisp :: (TH.Name, ParamSpec) -> TH.Exp
        makeParamDisp (pName, paramSpec) =
          TH.AppE (TH.VarE (TH.mkName dispFun)) (TH.VarE pName)
          where
            dispFun =
              case paramSpec of
                ParamInt -> "disp"
                ParamIntList -> "dispListLiteral"

        joinDocsDirect :: TH.Exp -> TH.Exp -> TH.Exp
        joinDocsDirect e1 =
          TH.AppE (TH.AppE (TH.VarE (TH.mkName "<>")) e1)

        joinDocsWithComma :: TH.Exp -> TH.Exp -> TH.Exp
        joinDocsWithComma e1 =
          TH.AppE (TH.AppE (TH.VarE (TH.mkName "<+>")) (joinDocsDirect e1 (string ",")))
