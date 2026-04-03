module TokenSpec (spec) where

import Common.TokenUtil (Located (..))
import Data.Text (Text)
import Staged.Token (Token (..))
import Staged.Token qualified as Token
import Test.Hspec
import Prelude hiding (lex)

lex :: Text -> Either String [Token]
lex = fmap (map (\(Located _ v) -> v)) . Token.lex

spec :: Spec
spec = do
  describe "lex" $ do
    it "tokenizes &&" $ do
      lex "x == 1 && b"
        `shouldBe` pure [TokLower "x", TokOpComp "==", TokInt 1, TokOpAnd "&&", TokLower "b"]
    it "tokenizes /" $ do
      lex "float -1 / x"
        `shouldBe` pure [TokLower "float", TokInt (-1), TokOpMult "/", TokLower "x"]
    it "tokenizes ?foo" $ do
      lex "?foo Int -> Bool"
        `shouldBe` pure [TokLabelOmissible "foo", TokUpper "Int", TokArrow, TokUpper "Bool"]
