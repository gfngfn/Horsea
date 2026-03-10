module Data.List.TwoOrMore
  ( TwoOrMore,
    make,
    decompose,
    fromNonEmpty,
    head,
    last,
    initAndLast,
    foldl1,
  )
where

import Data.Functor.Classes (Eq1, Show1)
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Generic.Data (Generic, Generic1, Generically1 (..))
import Generic.Data.Orphans ()
import Prelude hiding (foldl1, head, last)

data TwoOrMore a = TwoOrMore
  { first :: a,
    rest :: NonEmpty a
  }
  deriving stock (Eq, Ord, Generic, Generic1, Show, Functor, Foldable, Traversable)
  deriving (Eq1, Show1) via (Generically1 TwoOrMore)

make :: a -> a -> [a] -> TwoOrMore a
make first second rest' =
  TwoOrMore {first, rest = second :| rest'}

decompose :: TwoOrMore a -> (a, a, [a])
decompose TwoOrMore {first, rest = second :| rest'} =
  (first, second, rest')

fromNonEmpty :: NonEmpty a -> Maybe (TwoOrMore a)
fromNonEmpty (first :| rest') = do
  rest <- nonEmpty rest'
  pure TwoOrMore {first, rest}

head :: TwoOrMore a -> a
head xs = xs.first

last :: TwoOrMore a -> a
last xs = NonEmpty.last xs.rest

initAndLast :: TwoOrMore a -> (NonEmpty a, a)
initAndLast TwoOrMore {first, rest} =
  (first :| NonEmpty.init rest, NonEmpty.last rest)

foldl1 :: (a -> a -> a) -> TwoOrMore a -> a
foldl1 f TwoOrMore {first, rest} = foldl f first rest
