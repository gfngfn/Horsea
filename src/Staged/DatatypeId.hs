module Staged.DatatypeId
  ( DatatypeId,
    fresh,
    getName,
  )
where

import Data.Text (Text)
import Prelude

data DatatypeId = DatatypeId Int Text
  deriving (Show)

instance Eq DatatypeId where
  (==) (DatatypeId i1 _) (DatatypeId i2 _) =
    i1 == i2

instance Ord DatatypeId where
  compare (DatatypeId i1 _) (DatatypeId i2 _) =
    compare i1 i2

fresh :: Int -> Text -> DatatypeId
fresh index name =
  DatatypeId index name

getName :: DatatypeId -> Text
getName (DatatypeId _ name) = name
