module TokenSpec (spec) where

import Data.Text (Text)
import Staged.Token (Token (..))
import Staged.Token qualified as Token
import Test.Hspec
import Util.TokenUtil (Located (..))
import Prelude hiding (lex)

lex :: Text -> Either String [Token]
lex = fmap (map (\(Located _ v) -> v)) . Token.lex

spec :: Spec
spec = do
  describe "lex" $ do
    it "tokenizes &&" $ do
      lex "x == 1 && b"
        `shouldBe` pure [TokLower "x", TokOpComp "==", TokInt 1, TokOpAnd "&&", TokLower "b"]
