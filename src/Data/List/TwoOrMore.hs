module Data.List.TwoOrMore
  ( TwoOrMore,
    make,
    make1,
    decompose,
    decompose1,
    fromNonEmpty,
    toList,
    fromList,
    zipExact,
    mapIndexed,
    head,
    last,
    initAndLast,
    foldl1,
    transpose,
  )
where

import Data.Functor.Classes (Eq1, Show1)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.NonEmpty.Util qualified as U
import Generic.Data (Generic, Generic1, Generically1 (..))
import Generic.Data.Orphans ()
import Safe.Exact (zipExactMay)
import Prelude hiding (foldl1, head, last)

data TwoOrMore a = TwoOrMore
  { first :: a,
    rest :: NonEmpty a
  }
  deriving stock (Eq, Ord, Generic, Generic1, Show, Functor, Foldable, Traversable)
  deriving (Eq1, Show1) via (Generically1 TwoOrMore)

make :: a -> a -> [a] -> TwoOrMore a
make first second rest' = TwoOrMore {first, rest = second :| rest'}

make1 :: a -> NonEmpty a -> TwoOrMore a
make1 first rest = TwoOrMore {first, rest}

decompose :: TwoOrMore a -> (a, a, [a])
decompose TwoOrMore {first, rest = second :| rest'} =
  (first, second, rest')

decompose1 :: TwoOrMore a -> (a, NonEmpty a)
decompose1 TwoOrMore {first, rest} = (first, rest)

fromNonEmpty :: NonEmpty a -> Maybe (TwoOrMore a)
fromNonEmpty (first :| rest') = do
  rest <- nonEmpty rest'
  pure TwoOrMore {first, rest}

toList :: TwoOrMore a -> [a]
toList TwoOrMore {first, rest} = first : NonEmpty.toList rest

fromList :: [a] -> Maybe (TwoOrMore a)
fromList = \case
  x1 : x2 : xs -> pure $ make x1 x2 xs
  _ -> Nothing

zipExact :: TwoOrMore a -> TwoOrMore b -> Maybe (TwoOrMore (a, b))
zipExact xs ys = do
  rest <- zipExactMay xsRest ysRest
  pure $ make (x1, y1) (x2, y2) rest
  where
    (x1, x2, xsRest) = decompose xs
    (y1, y2, ysRest) = decompose ys

mapIndexed :: (Int -> a -> b) -> TwoOrMore a -> TwoOrMore b
mapIndexed f TwoOrMore {first, rest = second :| others} =
  TwoOrMore
    { first = f 0 first,
      rest = f 1 second :| zipWith f [2 ..] others
    }

head :: TwoOrMore a -> a
head xs = xs.first

last :: TwoOrMore a -> a
last xs = NonEmpty.last xs.rest

initAndLast :: TwoOrMore a -> (NonEmpty a, a)
initAndLast TwoOrMore {first, rest} =
  (first :| NonEmpty.init rest, NonEmpty.last rest)

foldl1 :: (a -> a -> a) -> TwoOrMore a -> a
foldl1 f TwoOrMore {first, rest} = foldl f first rest

transpose :: NonEmpty (TwoOrMore a) -> Maybe (TwoOrMore (NonEmpty a))
transpose matrix = do
  matrix' <- U.transpose (fmap toList matrix)
  case fromList matrix' of
    Nothing -> error "bug: Data.List.TwoOrMore"
    Just matrix'' -> pure matrix''
