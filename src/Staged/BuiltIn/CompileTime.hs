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
  )
where

import Data.Maybe (mapMaybe)
import Language.Haskell.TH qualified as TH
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
  let dec0s = map (derivePerArity allBiSpecs) [1 .. 1]
  -- TODO: change "Foo" to "Ass1BuiltIn":
  let dec1 = TH.DataD [] (TH.mkName "Foo") [] Nothing (mapMaybe makeConstructor1 allBiSpecs) derivClauses
  let decs = dec1 : dec0s
  TH.runIO $ putStrLn $ "DERIVED: " ++ show decs
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

derivePerArity :: [BuiltInSpec] -> Int -> TH.Dec
derivePerArity allBiSpecs arity =
  TH.DataD [] builtInArityName [] Nothing (map makeConstructor0 pairs) derivClauses
  where
    builtInArityName = TH.mkName $ "BuiltInArity" ++ show arity

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
