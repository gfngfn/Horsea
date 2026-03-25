module Data.List.NonEmpty.Util
  ( transpose,
  )
where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Prelude

-- | Reference: `Data.List.transpose`
transpose :: NonEmpty [a] -> [NonEmpty a]
transpose = \case
  [] :| xss ->
    maybe [] transpose (nonEmpty xss)
  (x : xs) :| xss ->
    combine x hds xs tls
    where
      (hds, tls) = unzip [(hd, tl) | hd : tl <- xss]
      combine y h ys t = (y :| h) : transpose (ys :| t)
