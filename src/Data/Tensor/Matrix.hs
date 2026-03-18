module Data.Tensor.Matrix
  ( Matrix,
    ConstructionError (..),
    fromRows,
    toRows,
    size,
    transpose,
    mult,
    add,
    concatVert,
  )
where

import Data.List qualified as List
import Safe.Exact
import Prelude

newtype Matrix = Matrix [[Int]]
  deriving newtype (Eq, Show)

data ConstructionError
  = EmptyRow
  | InconsistencyOfRowLength [Int] [Int]
  deriving stock (Eq, Show)

fromRows :: [[Int]] -> Either ConstructionError Matrix
fromRows [] = pure $ Matrix []
fromRows rows@(firstRow : restRows) =
  if expectedLength == 0
    then Left EmptyRow -- Matrices of size (0, n) is not allowed where n > 0
    else case List.find (\row -> List.length row /= expectedLength) restRows of
      Just row' -> Left $ InconsistencyOfRowLength firstRow row'
      Nothing -> pure $ Matrix rows
  where
    expectedLength = List.length firstRow

toRows :: Matrix -> [[Int]]
toRows (Matrix rows) = rows

size :: Matrix -> (Int, Int)
size (Matrix []) = (0, 0)
size (Matrix rows@(firstRow : _)) = (List.length rows, List.length firstRow)

transpose :: Int -> Int -> Matrix -> Maybe Matrix
transpose 0 0 (Matrix []) = pure $ Matrix []
transpose m n _ | m <= 0 || n <= 0 = Nothing
transpose m n (Matrix rows)
  | m == List.length rows =
      Matrix <$> go [] n rows
  where
    go :: [[Int]] -> Int -> [[Int]] -> Maybe [[Int]]
    go _ k _ | k < 0 = Nothing
    go acc 0 rows0 =
      if List.all null rows0
        then pure $ reverse acc
        else Nothing
    go acc k rows0 = do
      firstAndRestPairs <- mapM List.uncons rows0
      let column = map fst firstAndRestPairs
      let rows1 = map snd firstAndRestPairs
      go (column : acc) (k - 1) rows1
transpose _ _ _ = Nothing

mult :: Int -> Int -> Int -> Matrix -> Matrix -> Maybe Matrix
mult k m n mat1 mat2 = do
  let Matrix rows1 = mat1 -- `rows1`: a list of length k
  Matrix columns2 <- transpose m n mat2 -- `columns2`: a list of length n
  if size mat1 == (k, m) && size mat2 == (m, n)
    -- `mat1`: a matrix of size k × m
    -- `mat2`: a matrix of size m × n
    then pure $ Matrix [[calc row1 column2 | row1 <- rows1] | column2 <- columns2]
    else Nothing
  where
    calc :: [Int] -> [Int] -> Int
    calc row column = List.foldl' (+) 0 $ zipWith (*) row column

add :: Int -> Int -> Matrix -> Matrix -> Maybe Matrix
add m n mat1 mat2 = do
  let Matrix rows1 = mat1
  let Matrix rows2 = mat2
  if size mat1 == (m, n) && size mat2 == (m, n)
    then do
      zippedRows <- zipExactMay rows1 rows2
      Matrix
        <$> mapM
          (\(elems1, elems2) -> map (uncurry (+)) <$> zipExactMay elems1 elems2)
          zippedRows
    else
      Nothing

concatVert :: Int -> Int -> Int -> Matrix -> Matrix -> Maybe Matrix
concatVert m1 m2 n mat1 mat2 =
  if size mat1 == (m1, n) && size mat2 == (m2, n)
    then pure $ Matrix (rows1 ++ rows2)
    else Nothing
  where
    Matrix rows1 = mat1
    Matrix rows2 = mat2
