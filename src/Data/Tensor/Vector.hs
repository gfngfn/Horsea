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

newtype Vector = Vector [Int]
  deriving newtype (Eq, Show)

fromList :: [Int] -> Vector
fromList = Vector

toList :: Vector -> [Int]
toList (Vector elems) = elems

length :: Vector -> Int
length (Vector elems) = List.length elems

-- A naive emulation of vector addition
add :: Int -> Vector -> Vector -> Maybe Vector
add n (Vector v1) (Vector v2) =
  if List.length v1 == n && List.length v2 == n
    then Just . Vector $ List.zipWith (+) v1 v2
    else Nothing

-- A naive emulation of vector concatenation
concat :: Int -> Int -> Vector -> Vector -> Maybe Vector
concat m n (Vector v1) (Vector v2) =
  if List.length v1 == m && List.length v2 == n
    then Just . Vector $ v1 ++ v2
    else Nothing
