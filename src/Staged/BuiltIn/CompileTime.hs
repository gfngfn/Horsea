{-# LANGUAGE TemplateHaskell #-}
module Staged.BuiltIn.CompileTime
  ( BuiltInSpec (..),
    ParamSpec (..),
    derive,
  )
where

import Data.Text (Text)
import Language.Haskell.TH qualified as TH
import Prelude

data BuiltInSpec = BuiltInSpec
  { name :: Text,
    constructor :: String,
    params :: [ParamSpec]
  }

data ParamSpec
  = ParamInt
  | ParamIntList

derive :: [BuiltInSpec] -> TH.Q [TH.Dec]
derive biSpecs =
  concat <$> mapM (derivePerArity biSpecs) [1..8]

derivePerArity :: [BuiltInSpec] -> Int -> TH.Q [TH.Dec]
derivePerArity allBiSpecs arity = do
  let biSpecs = filter (\biSpec -> length biSpec.params == arity) allBiSpecs
  let builtInArityName = TH.mkName $ "BuiltInArity" ++ show arity
  pure
    [ TH.DataD [] builtInArityName [] Nothing (map deriveConstructor biSpecs) []
    ]
  where
    deriveConstructor :: BuiltInSpec -> TH.Con
    deriveConstructor BuiltInSpec {constructor, params} =
      TH.NormalC (TH.mkName constructor) (map ((noBang,) . deriveParam) params)

    noBang :: TH.Bang
    noBang = TH.Bang TH.NoSourceUnpackedness TH.NoSourceStrictness

    deriveParam :: ParamSpec -> TH.Type
    deriveParam = \case
      ParamInt -> TH.ConT ''Int
      ParamIntList -> TH.AppT (TH.ConT ''[]) (TH.ConT ''Int)
