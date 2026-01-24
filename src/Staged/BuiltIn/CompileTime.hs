{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.CompileTime
  ( BuiltInSpec (..),
    BuiltInSpecMain (..),
    GenSpec (..),
    VersatileSpec (..),
    ParamSpec (..),
    gen,
    versatile,
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
  { constructor0 :: String,
    main :: BuiltInSpecMain
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

gen :: String -> GenSpec -> BuiltInSpec
gen constructor0 genSpec = BuiltInSpec {constructor0, main = Gen genSpec}

versatile :: String -> VersatileSpec -> BuiltInSpec
versatile constructor0 versSpec = BuiltInSpec {constructor0, main = Versatile versSpec}

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
        ( \BuiltInSpec {constructor0, main} ->
            case main of
              Gen genSpec ->
                if length genSpec.params == arity then Just (constructor0, []) else Nothing
              Versatile versSpec ->
                if versSpec.arity == arity then Just (constructor0, versSpec.fixedParams) else Nothing
        )
        allBiSpecs

    derivClauses :: [TH.DerivClause]
    derivClauses = [TH.DerivClause (Just TH.StockStrategy) [TH.ConT ''Eq, TH.ConT ''Show]]

    makeConstructor0 :: (String, [ParamSpec]) -> TH.Con
    makeConstructor0 (constructor0, fixedParams) =
      TH.NormalC (TH.mkName constructor0) (map ((noBang,) . makeParam) fixedParams)

noBang :: TH.Bang
noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

makeParam :: ParamSpec -> TH.Type
makeParam = \case
  ParamInt -> TH.ConT ''Int
  ParamIntList -> TH.AppT (TH.ConT ''[]) (TH.ConT ''Int)

filterGen :: [BuiltInSpec] -> [(String, GenSpec)]
filterGen =
  mapMaybe
    ( \BuiltInSpec {constructor0, main} ->
        case main of
          Gen genSpec -> Just (constructor0, genSpec)
          Versatile _ -> Nothing
    )

filterVersatile :: [BuiltInSpec] -> [(String, VersatileSpec)]
filterVersatile =
  mapMaybe
    ( \BuiltInSpec {constructor0, main} ->
        case main of
          Gen _ -> Nothing
          Versatile versSpec -> Just (constructor0, versSpec)
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
    makeGenBranch :: (String, GenSpec) -> Maybe TH.Match
    makeGenBranch (constructor0, genSpec) = do
      zipped <- zipExactMay genSpec.params namePairs
      let branchBody = TH.DoE Nothing (map makeStmt zipped ++ [TH.NoBindS (TH.AppE (TH.VarE 'pure) retVal)])
      pure $ TH.Match pat (TH.NormalB branchBody) []
      where
        pat :: TH.Pat
        pat = TH.ConP (TH.mkName constructor0) [] []

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
    makeVersatileBranch :: (String, VersatileSpec) -> TH.Q (Maybe TH.Match)
    makeVersatileBranch (constructor0, versSpec) = do
      if versSpec.arity /= arity
        then
          pure Nothing
        else do
          branchBody <- versSpec.bodyQ
          pure . Just $ TH.Match pat (TH.NormalB branchBody) []
      where
        pat = TH.ConP (TH.mkName constructor0) [] (map TH.VarP fixedParamVars)
        fixedParamVars = map (\i -> TH.mkName ("p" ++ show i)) [1 .. length versSpec.fixedParams]

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

    makeGenBranch :: (String, GenSpec) -> Maybe TH.Match
    makeGenBranch (constructor0, genSpec) =
      if length genSpec.params /= arity
        then Nothing
        else pure $ TH.Match pat (TH.NormalB branchBody) []
      where
        pat :: TH.Pat
        pat = TH.ConP (TH.mkName constructor0) [] []

        branchBody :: TH.Exp
        branchBody = string constructorDisp

        constructorDisp = constructor0 -- TODO: make this upper-camelcased

    makeVersatileBranch :: (String, VersatileSpec) -> Maybe TH.Match
    makeVersatileBranch (constructor0, versSpec) = do
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
        pat = TH.ConP (TH.mkName constructor0) [] (map (TH.VarP . fst) fixedParamVars)

        branchBody :: TH.Exp
        branchBody =
          case nonEmpty fixedParamVars of
            Nothing ->
              string constructorDisp
            Just fixedParamVarsNonEmpty ->
              joinDocsDirect
                (string constructorDisp)
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

        constructorDisp = constructor0 -- TODO: make this upper-camelcased

        joinDocsDirect :: TH.Exp -> TH.Exp -> TH.Exp
        joinDocsDirect e1 =
          TH.AppE (TH.AppE (TH.VarE (TH.mkName "<>")) e1)

        joinDocsWithComma :: TH.Exp -> TH.Exp -> TH.Exp
        joinDocsWithComma e1 =
          TH.AppE (TH.AppE (TH.VarE (TH.mkName "<+>")) (joinDocsDirect e1 (string ",")))
