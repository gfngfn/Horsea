module Staged.Core
  ( Label,
    ConstructorName,
    DatasetParam (..),
  )
where

import Data.Text (Text)
import Prelude

type Label = Text

type ConstructorName = Text

data DatasetParam f a = DatasetParam
  { numTrain :: a,
    numTest :: a,
    image :: f a,
    label :: f a
  }
  deriving stock (Eq, Show, Functor)
