module NonEmptyUtilSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Util
import Test.Hspec

spec :: Spec
spec = do
  describe "transpose" $ do
    it "case 1" $
      transpose (([1, 2, 3] :| [[4, 5, 6]]) :: NonEmpty [Int])
        `shouldBe` [1 :| [4], 2 :| [5], 3 :| [6]]
