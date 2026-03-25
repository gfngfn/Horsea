module Data.List.NonEmpty.Util
  ( transpose,
  )
where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Prelude

-- | Reference: `Data.List.transpose`
transpose :: NonEmpty [a] -> Maybe [NonEmpty a]
transpose matrix@(row' :| xss') =
  if all (\r -> length r == len) xss'
    then pure $ go matrix
    else Nothing
  where
    len = length row'
    go (row :| xss) =
      case row of
        [] -> maybe [] go (nonEmpty xss)
        x : xs -> (x :| hds) : go (xs :| tls)
          where
            (hds, tls) = unzip [(hd, tl) | hd : tl <- xss]
