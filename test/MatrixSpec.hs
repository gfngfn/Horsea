module MatrixSpec (spec) where

import Data.Tensor.Matrix (Matrix)
import Data.Tensor.Matrix qualified as Matrix
import Test.Hspec

make :: [[Int]] -> Matrix Int
make rows =
  case Matrix.fromRows rows of
    Left _ -> error "make"
    Right mat -> mat

spec :: Spec
spec = do
  describe "Matrix.transpose" $ do
    it "computes transpositions (1)" $
      Matrix.transpose 2 2 (make [[1, 2], [3, 4]])
        `shouldBe` Just (make [[1, 3], [2, 4]])
    it "computes transpositions (2)" $
      Matrix.transpose 2 3 (make [[1, 2, 3], [4, 5, 6]])
        `shouldBe` Just (make [[1, 4], [2, 5], [3, 6]])
  describe "Matrix.mult" $ do
    it "computes multiplications (1)" $
      -- 1*8 + 2*9 + 3*10 = 56
      -- 4*8 + 5*9 + 6*10 = 137
      Matrix.mult 2 3 1 (make [[1, 2, 3], [4, 5, 6]]) (make [[8], [9], [10]])
        `shouldBe` Just (make [[56, 137]])
    it "computes multiplications (2)" $
      -- 1*0 + 2*11 + 3*7 = 43
      -- 4*0 + 5*11 + 6*7 = 97
      Matrix.mult 2 3 2 (make [[1, 2, 3], [4, 5, 6]]) (make [[8, 0], [9, 11], [10, 7]])
        `shouldBe` Just (make [[56, 137], [43, 97]])
