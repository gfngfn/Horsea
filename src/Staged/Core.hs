module Staged.Core
  ( Label,
    DatasetParam (..),
  )
where

import Data.Text (Text)
import Prelude

type Label = Text

data DatasetParam f a = DatasetParam
  { numTrain :: a,
    numTest :: a,
    image :: f a,
    label :: f a
  }
  deriving stock (Eq, Show, Functor)
