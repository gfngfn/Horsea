{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.CompileTime
  ( BuiltInSpec (..),
    ParamSpec (..),
    derive,
  )
where

import Language.Haskell.TH qualified as TH
import Prelude

data BuiltInSpec = BuiltInSpec
  { constructor0 :: String,
    params :: [ParamSpec],
    constructor1 :: String
  }

data ParamSpec
  = ParamInt
  | ParamIntList

derive :: [BuiltInSpec] -> TH.Q [TH.Dec]
derive allBiSpecs = do
  let dec0s = map (derivePerArity allBiSpecs) [1 .. 1]
  -- TODO: change "Foo" to "Ass1BuiltIn":
  let dec1 = TH.DataD [] (TH.mkName "Foo") [] Nothing (map makeConstructor1 allBiSpecs) derivClauses
  let decs = dec1 : dec0s
  TH.runIO $ putStrLn $ "DERIVED: " ++ show decs
  pure decs
  where
    derivClauses :: [TH.DerivClause]
    derivClauses = [TH.DerivClause (Just TH.StockStrategy) [TH.ConT ''Eq, TH.ConT ''Show]]

    makeConstructor1 :: BuiltInSpec -> TH.Con
    makeConstructor1 BuiltInSpec {constructor1, params} =
      -- TODO: remove "Bar":
      TH.NormalC (TH.mkName ("Bar" ++ constructor1)) (map ((noBang,) . makeParam) params)

    noBang :: TH.Bang
    noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

    makeParam :: ParamSpec -> TH.Type
    makeParam = \case
      ParamInt -> TH.ConT ''Int
      ParamIntList -> TH.AppT (TH.ConT ''[]) (TH.ConT ''Int)

derivePerArity :: [BuiltInSpec] -> Int -> TH.Dec
derivePerArity allBiSpecs arity =
  TH.DataD [] builtInArityName [] Nothing (map makeConstructor0 biSpecs) derivClauses
  where
    biSpecs = filter (\biSpec -> length biSpec.params == arity) allBiSpecs
    builtInArityName = TH.mkName $ "BuiltInArity" ++ show arity

    derivClauses :: [TH.DerivClause]
    derivClauses = [TH.DerivClause (Just TH.StockStrategy) [TH.ConT ''Eq, TH.ConT ''Show]]

    makeConstructor0 :: BuiltInSpec -> TH.Con
    makeConstructor0 BuiltInSpec {constructor0} =
      TH.NormalC (TH.mkName constructor0) []
