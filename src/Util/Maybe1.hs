module Util.Maybe1
  ( Maybe1 (..),
    unMaybe1,
  )
where

import Prelude

newtype Maybe1 af sv = Maybe1 (Maybe (af sv))

unMaybe1 :: Maybe1 af sv -> Maybe (af sv)
unMaybe1 (Maybe1 x) = x
