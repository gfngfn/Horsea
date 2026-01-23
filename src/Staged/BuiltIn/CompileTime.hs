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
  )
where

import Data.List (foldl')
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
    arity :: Int
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

deriveDeltaReduction :: [BuiltInSpec] -> TH.Q [TH.Dec]
deriveDeltaReduction allBiSpecs =
  pure $ map (deriveDeltaReductionPerArity allBiSpecs) [1 .. 1]

deriveDeltaReductionPerArity :: [BuiltInSpec] -> Int -> TH.Dec
deriveDeltaReductionPerArity allBiSpecs arity =
  TH.FunD reduceDeltaArityFunName [TH.Clause (map TH.VarP (biName : argValNames)) (TH.NormalB body) []]
  where
    reduceDeltaArityFunName = TH.mkName $ "reduceDeltaArity" ++ show arity
    biName = TH.mkName $ "bi" ++ show arity
    namePairs = map (\i -> (TH.mkName ("x" ++ show i), TH.mkName ("a0v" ++ show i))) [1 .. arity]
    validatedValNames = map fst namePairs
    argValNames = map snd namePairs

    allGenPairs =
      mapMaybe
        ( \BuiltInSpec {constructor0, main} ->
            case main of
              Gen genSpec -> Just (constructor0, genSpec)
              Versatile _ -> Nothing
        )
        allBiSpecs

    -- The body of the function `reduceDeltaArity{N}`.
    body :: TH.Exp
    body = TH.CaseE (TH.VarE biName) (mapMaybe makeGenBranch allGenPairs)

    -- Constructs a branch for handling one *Gen-based* built-in function.
    makeGenBranch :: (String, GenSpec) -> Maybe TH.Match
    makeGenBranch (constructor0, genSpec) = do
      zipped <- zipExactMay genSpec.params namePairs
      let branchBody = TH.DoE Nothing (map makeStmt zipped ++ [TH.NoBindS (TH.AppE (TH.VarE 'pure) retVal)])
      pure $ TH.Match (TH.ConP (TH.mkName constructor0) [] []) (TH.NormalB branchBody) []
      where
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
                (TH.VarE (TH.mkName genSpec.constructor1))
                (map TH.VarE validatedValNames)
