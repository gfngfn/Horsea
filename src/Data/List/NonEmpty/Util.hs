module Data.List.NonEmpty.Util
  ( transpose,
  )
where

import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Prelude

-- | Reference: `Data.List.transpose`
transpose :: NonEmpty [a] -> [NonEmpty a]
transpose (row :| xss) =
  case row of
    [] -> maybe [] transpose (nonEmpty xss)
    x : xs -> (x :| hds) : transpose (xs :| tls)
      where
        (hds, tls) = unzip [(hd, tl) | hd : tl <- xss]

{-
transposeTwoOrMore :: NonEmpty (TwoOrMore a) -> TwoOrMore (NonEmpty a)
transposeTwoOrMore (row :| xss) =
  let (x, xs) = TwoOrMore.decompose1 row in
  TwoOrMore.make1 (x :| hds) (transposeTwoOrMore (xs :| tls))
  where
    (hds, tls) = unzip [(hd, tl) | (hd, tl) <- map TwoOrMore.decompose1 xss]

class LowerBoundedSeq fMin f | f -> fMin where
  lbsUncons :: f a -> Either (fMin a) (a, f a)
  lbsMake :: fMin a -> f a

instance LowerBoundedSeq Identity NonEmpty where
  lbsUncons (x :| xs) =
    case nonEmpty xs of
      Nothing -> Left (Identity x)
      Just xs' -> Right (x, xs')
  lbsMake (Identity x) = x :| []

transpose' :: forall fMin f a. (Cons f, LowerBoundedSeq fMin f) => NonEmpty (f a) -> f (NonEmpty a)
transpose' (row :| xss) =
  case lbsUncons row of
    Left (m :: fMin a) -> maybe [] transpose' (nonEmpty xss)
    Right (x, xs) -> (x :| hds) `cons` transpose (xs :| tls)
    where
      (hds, tls) = unzip [(hd, tl) | Right (hd, tl) <- map lbsUncons xss]
-}
