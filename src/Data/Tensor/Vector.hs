module Data.Tensor.Vector
  ( Vector,
    fromList,
    toList,
    length,
    add,
    concat,
  )
where

import Data.List qualified as List
import Prelude hiding (concat, length)

newtype Vector a = Vector [a]
  deriving newtype (Eq, Show)

fromList :: [a] -> Vector a
fromList = Vector

toList :: Vector a -> [a]
toList (Vector elems) = elems

length :: Vector a -> Int
length (Vector elems) = List.length elems

-- A naive emulation of vector addition
add :: (Num a) => Int -> Vector a -> Vector a -> Maybe (Vector a)
add n (Vector v1) (Vector v2) =
  if List.length v1 == n && List.length v2 == n
    then Just . Vector $ List.zipWith (+) v1 v2
    else Nothing

-- A naive emulation of vector concatenation
concat :: Int -> Int -> Vector a -> Vector a -> Maybe (Vector a)
concat m n (Vector v1) (Vector v2) =
  if List.length v1 == m && List.length v2 == n
    then Just . Vector $ v1 ++ v2
    else Nothing
