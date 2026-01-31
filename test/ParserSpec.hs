module ParserSpec (spec) where

import Data.Functor
import Data.Text (Text)
import Staged.Parser qualified as Parser
import Staged.SrcSyntax
import SyntaxUtil
import Test.Hspec
import Util.FrontError (FrontError)
import Util.LocationInFile (SourceSpec (..))
import Util.TokenUtil (Span (..))

parseExpr :: Text -> Either FrontError ExprVoid
parseExpr s = fmap void (Parser.parseExpr (SourceSpec s "test") s)

parseExprWithLoc :: Text -> Either FrontError Expr
parseExprWithLoc s = Parser.parseExpr (SourceSpec s "test") s

parseTypeExpr :: Text -> Either FrontError TypeExprVoid
parseTypeExpr s = fmap void (Parser.parseTypeExpr (SourceSpec s "test") s)

parseBinds :: Text -> Either FrontError [BindVoid]
parseBinds s = fmap (map void) (Parser.parseBinds (SourceSpec s "test") s)

exprLoc :: Int -> Int -> ExprMainF Span -> Expr
exprLoc start end = Expr (Span start end)

typLoc :: Int -> Int -> TypeExprMainF Span -> TypeExpr
typLoc start end = TypeExpr (Span start end)

spec :: Spec
spec = do
  describe "parseExpr (without code locations)" $ do
    it "parses integer literals (1)" $
      parseExpr "42"
        `shouldBe` pure (litInt 42)
    it "parses integer literals (2)" $
      parseExpr "0"
        `shouldBe` pure (litInt 0)
    it "parses integer literals (3)" $
      parseExpr "-42"
        `shouldBe` pure (litInt (-42))
    it "parses float literals (1)" $
      parseExpr "0.0"
        `shouldBe` pure (litFloat 0.0)
    it "parses float literals (2)" $
      parseExpr "0.9"
        `shouldBe` pure (litFloat 0.9)
    it "parses float literals (3)" $
      parseExpr "0.57"
        `shouldBe` pure (litFloat 0.57)
    it "parses float literals (4)" $
      parseExpr "0.001"
        `shouldBe` pure (litFloat 0.001)
    it "parses float literals (5)" $
      parseExpr "44.23"
        `shouldBe` pure (litFloat 44.23)
    it "parses float literals (6)" $
      parseExpr "42.0"
        `shouldBe` pure (litFloat 42)
    it "parses list literals (1)" $
      parseExpr "[]"
        `shouldBe` pure (litList [])
    it "parses list literals (2)" $
      parseExpr "[n + 1, x y]"
        `shouldBe` pure (litList [add (var "n") (litInt 1), app (var "x") (var "y")])
    it "parses vector literals (1)" $
      parseExpr "[| |]"
        `shouldBe` pure (litVec [])
    it "parses vector literals (2)" $
      parseExpr "[| 3; 14; 1592 |]"
        `shouldBe` pure (litVec [3, 14, 1592])
    it "parses variables (1)" $
      parseExpr "x"
        `shouldBe` pure (var "x")
    it "parses variables (2)" $
      parseExpr "foo_bar"
        `shouldBe` pure (var "foo_bar")
    it "parses long variables (1)" $
      parseExpr "Foo.x"
        `shouldBe` pure (longVar ["Foo"] "x")
    it "parses long variables (2)" $
      parseExpr "Foo.Bar.x"
        `shouldBe` pure (longVar ["Foo", "Bar"] "x")
    it "parses applications (1)" $
      parseExpr "x y"
        `shouldBe` pure (app (var "x") (var "y"))
    it "parses applications (2)" $
      parseExpr "x y z"
        `shouldBe` pure (app (app (var "x") (var "y")) (var "z"))
    it "parses applications (3)" $
      parseExpr "x (y z)"
        `shouldBe` pure (app (var "x") (app (var "y") (var "z")))
    it "parses applications (4)" $ do
      let eBody = expr (LetIn "z" [] (litInt 1) (app (var "y") (var "z")))
      parseExpr "Foo.bar (fun(y : Y) -> let z = 1 in y z)"
        `shouldBe` pure (app (longVar ["Foo"] "bar") (nonrecLam ("y", typ (TyName "Y" [])) eBody))
    it "parses applications with labels (1)" $
      parseExpr "x #foo y"
        `shouldBe` pure (appWithLabel (var "x") "foo" (var "y"))
    it "parses applications with labels (2)" $
      parseExpr "x y #foo z"
        `shouldBe` pure (appWithLabel (app (var "x") (var "y")) "foo" (var "z"))
    it "parses applications with labels (3)" $
      parseExpr "x (y #foo z)"
        `shouldBe` pure (app (var "x") (appWithLabel (var "y") "foo" (var "z")))
    it "parses applications and integer literals" $
      parseExpr "x 42 z"
        `shouldBe` pure (app (app (var "x") (litInt 42)) (var "z"))
    it "parses applications with labels and integer literals" $
      parseExpr "x #foo 42 z"
        `shouldBe` pure (app (appWithLabel (var "x") "foo" (litInt 42)) (var "z"))
    it "parses applications and integer/float literals" $
      parseExpr "x 42 5.7"
        `shouldBe` pure (app (app (var "x") (litInt 42)) (litFloat 5.7))
    it "parses lambda abstractions (1)" $
      parseExpr "fun (x : Int) -> x"
        `shouldBe` pure (nonrecLam ("x", tyInt) (var "x"))
    it "parses lambda abstractions (2)" $ do
      let ty = tyDepFun "n" tyInt tyBool
      parseExpr "fun (x : (n : Int) -> Bool) -> x y"
        `shouldBe` pure (nonrecLam ("x", ty) (app (var "x") (var "y")))
    it "parses recursive lambda abstractions" $
      parseExpr "rec (self : Int -> Int) -> fun (x : Int) -> x"
        `shouldBe` pure (recLam ("self", tyNondepFun tyInt tyInt) ("x", tyInt) (var "x"))
    it "parses let-expressions (1)" $ do
      let ty = tyDepFun "n" tyInt tyBool
      parseExpr "let f = fun (x : (n : Int) -> Bool) -> x y in f"
        `shouldBe` pure (expr (LetIn "f" [] (nonrecLam ("x", ty) (app (var "x") (var "y"))) (var "f")))
    it "parses let-expressions (2)" $ do
      let ty = tyDepFun "n" tyInt tyBool
      parseExpr "let f (x : (n : Int) -> Bool) = x y in f"
        `shouldBe` pure (expr (LetIn "f" [MandatoryBinder Nothing ("x", ty)] (app (var "x") (var "y")) (var "f")))
    it "parses let-expressions (3)" $ do
      let params = [ImplicitBinder ("n", tyInt), MandatoryBinder Nothing ("x", tyPersVec (var "n"))]
      parseExpr "let f {n : Int} (x : Vec %n) = g x in f"
        `shouldBe` pure (expr (LetIn "f" params (app (var "g") (var "x")) (var "f")))
    it "parses let-expressions (4)" $ do
      let params = [ImplicitBinder ("n", tyInt), MandatoryBinder (Just "foo") ("x", tyPersVec (var "n"))]
      parseExpr "let f {n : Int} #foo (x : Vec %n) = g x in f"
        `shouldBe` pure (expr (LetIn "f" params (app (var "g") (var "x")) (var "f")))
    it "parses let-open-expressions" $
      parseExpr "let open X in f 42"
        `shouldBe` pure (expr (LetOpenIn "X" (app (var "f") (litInt 42))))
    it "parses if-expressions" $
      parseExpr "if b then x + 1 else x"
        `shouldBe` pure (expr (IfThenElse (var "b") (add (var "x") (litInt 1)) (var "x")))
    it "parses brackets (1)" $
      parseExpr "f &x y"
        `shouldBe` pure (app (app (var "f") (bracket (var "x"))) (var "y"))
    it "parses brackets (2)" $
      parseExpr "f &(g x)"
        `shouldBe` pure (app (var "f") (bracket (app (var "g") (var "x"))))
    it "parses escapes (1)" $
      parseExpr "f ~x y"
        `shouldBe` pure (app (app (var "f") (escape (var "x"))) (var "y"))
    it "parses escapes (2)" $
      parseExpr "f ~(g x)"
        `shouldBe` pure (app (var "f") (escape (app (var "g") (var "x"))))
    it "parses binary operators (1)" $
      parseExpr "2 + 3"
        `shouldBe` pure (add (litInt 2) (litInt 3))
    it "parses binary operators (2)" $
      parseExpr "4 - 3 - 2"
        `shouldBe` pure (sub (sub (litInt 4) (litInt 3)) (litInt 2))
    it "parses binary operators (3)" $
      parseExpr "2 + 3 * 4"
        `shouldBe` pure (add (litInt 2) (mult (litInt 3) (litInt 4)))
    it "parses binary operators (4)" $
      parseExpr "f 2 + 3"
        `shouldBe` pure (add (app (var "f") (litInt 2)) (litInt 3))
    it "parses binary operators (5)" $
      parseExpr "2 + f 3"
        `shouldBe` pure (add (litInt 2) (app (var "f") (litInt 3)))
    it "parses binary operators (6)" $
      parseExpr "2 +=+ f 3"
        `shouldBe` pure (app (app (var "+=+") (litInt 2)) (app (var "f") (litInt 3)))
    it "parses binary operators (7)" $
      parseExpr "2 == f 3"
        `shouldBe` pure (app (app (var "==") (litInt 2)) (app (var "f") (litInt 3)))
    it "parses binary operators (8)" $
      parseExpr "2 <= f 3"
        `shouldBe` pure (app (app (var "<=") (litInt 2)) (app (var "f") (litInt 3)))
    it "parses binary operators (9)" $
      parseExpr "x == 1 && b"
        `shouldBe` pure (app (app (var "&&") (app (app (var "==") (var "x")) (litInt 1))) (var "b"))
    it "parses upcasts" $
      parseExpr "[| |] as Vec %n"
        `shouldBe` pure (upcast (litVec []) (tyPersVec (var "n")))
    it "parses optional applications (1)" $
      parseExpr "x {y}"
        `shouldBe` pure (appOptGiven (var "x") (var "y"))
    it "parses optional applications (2)" $
      parseExpr "x {y} {z}"
        `shouldBe` pure (appOptGiven (appOptGiven (var "x") (var "y")) (var "z"))
    it "parses optional applications (3)" $
      parseExpr "x {y + 1} {z}"
        `shouldBe` pure (appOptGiven (appOptGiven (var "x") (add (var "y") (litInt 1))) (var "z"))
    it "parses sequentials (1)" $
      parseExpr "x += 1; f x"
        `shouldBe` pure (expr (Sequential (app (app (var "+=") (var "x")) (litInt 1)) (app (var "f") (var "x"))))
    it "parses sequentials (2)" $
      parseExpr "x; y; z"
        `shouldBe` pure (expr (Sequential (var "x") (expr (Sequential (var "y") (var "z")))))
    it "parses sequentials (3)" $
      parseExpr "let x = y in f 42; z"
        `shouldBe` pure (expr (LetIn "x" [] (var "y") (expr (Sequential (app (var "f") (litInt 42)) (var "z")))))
    it "parses sequentials (4)" $
      parseExpr "f 42; let x = y in z"
        `shouldBe` pure (expr (Sequential (app (var "f") (litInt 42)) (expr (LetIn "x" [] (var "y") (var "z")))))
    it "parses sequentials (5)" $
      parseExpr "f 42; g 57; let x = y in z"
        `shouldBe` pure (expr (Sequential (app (var "f") (litInt 42)) (expr (Sequential (app (var "g") (litInt 57)) (expr (LetIn "x" [] (var "y") (var "z")))))))
    it "parses sequentials (6)" $
      parseExpr "f 42; if p 57 then x else y"
        `shouldBe` pure (expr (Sequential (app (var "f") (litInt 42)) (expr (IfThenElse (app (var "p") (litInt 57)) (var "x") (var "y")))))
    it "parses tuples (1)" $
      parseExpr "(x, y)"
        `shouldBe` pure (expr (Tuple (var "x") (var "y")))
    it "parses tuples (2)" $
      parseExpr "(f x, y + 1)"
        `shouldBe` pure (expr (Tuple (app (var "f") (var "x")) (add (var "y") (litInt 1))))
    it "parses tuples (3)" $
      parseExpr "(x, (y, z))"
        `shouldBe` pure (expr (Tuple (var "x") (expr (Tuple (var "y") (var "z")))))
    it "parses tuples (4)" $
      parseExpr "((x, y), z)"
        `shouldBe` pure (expr (Tuple (expr (Tuple (var "x") (var "y"))) (var "z")))
    it "parses let-tuples" $
      parseExpr "let (x, y) = s in t"
        `shouldBe` pure (expr (LetTupleIn "x" "y" (var "s") (var "t")))
    it "parses |> (1)" $
      parseExpr "x |> f"
        `shouldBe` pure (app (var "f") (var "x"))
    it "parses |> (2)" $
      parseExpr "x + 42 |> f |> g"
        `shouldBe` pure (app (var "g") (app (var "f") (add (var "x") (litInt 42))))
    it "ignores first comments" $
      parseExpr "(* comment *) 42"
        `shouldBe` pure (litInt 42)
    it "ignores first comments that include asterisks" $
      parseExpr "(* *comment* *) 42"
        `shouldBe` pure (litInt 42)
    it "ignores comments between applications" $
      parseExpr "f (* comment *) 42"
        `shouldBe` pure (app (var "f") (litInt 42))
  describe "Parser.parseTypeExpr" $ do
    it "parses dependent function types (1)" $
      parseTypeExpr "(n : Int) -> Bool"
        `shouldBe` pure (tyDepFun "n" tyInt tyBool)
    it "parses dependent function types (2)" $
      parseTypeExpr "(m : Int) -> (n : Int) -> Bool"
        `shouldBe` pure (tyDepFun "m" tyInt (tyDepFun "n" tyInt tyBool))
    it "parses dependent function types (3)" $
      parseTypeExpr "(f : (n : Int) -> Int) -> Bool"
        `shouldBe` pure (tyDepFun "f" (tyDepFun "n" tyInt tyInt) tyBool)
    it "parses dependent function types with labels" $
      parseTypeExpr "#foo (n : Int) -> Bool"
        `shouldBe` pure (tyDepFunWithLabel "foo" "n" tyInt tyBool)
    it "parses non-dependent function types (1)" $
      parseTypeExpr "Int -> Bool"
        `shouldBe` pure (tyNondepFun tyInt tyBool)
    it "parses non-dependent function types (2)" $
      parseTypeExpr "Int -> Int -> Bool"
        `shouldBe` pure (tyNondepFun tyInt (tyNondepFun tyInt tyBool))
    it "parses non-dependent function types (3)" $
      parseTypeExpr "(Int -> Int) -> Bool"
        `shouldBe` pure (tyNondepFun (tyNondepFun tyInt tyInt) tyBool)
    it "parses non-dependent function types with labels" $
      parseTypeExpr "#foo Int -> Bool"
        `shouldBe` pure (tyNondepFunWithLabel "foo" tyInt tyBool)
    it "parses mixed function types (1)" $
      parseTypeExpr "(m : Int) -> Int -> Bool"
        `shouldBe` pure (tyDepFun "m" tyInt (tyNondepFun tyInt tyBool))
    it "parses mixed function types (2)" $
      parseTypeExpr "Int -> (n : Int) -> Bool"
        `shouldBe` pure (tyNondepFun tyInt (tyDepFun "n" tyInt tyBool))
    it "parses mixed function types (3)" $
      parseTypeExpr "(f : Int -> Int) -> Bool"
        `shouldBe` pure (tyDepFun "f" (tyNondepFun tyInt tyInt) tyBool)
    it "parses mixed function types (4)" $
      parseTypeExpr "((n : Int) -> Int) -> Bool"
        `shouldBe` pure (tyNondepFun (tyDepFun "n" tyInt tyInt) tyBool)
    it "parses type applications (1)" $
      parseTypeExpr "Vec n"
        `shouldBe` pure (tyNormalVec (var "n"))
    it "parses type applications (2)" $
      parseTypeExpr "Vec %n"
        `shouldBe` pure (tyPersVec (var "n"))
    it "parses type applications (3)" $
      parseTypeExpr "(v : Vec n) -> Bool"
        `shouldBe` pure (tyDepFun "v" (tyNormalVec (var "n")) tyBool)
    it "parses type applications (4)" $
      parseTypeExpr "Vec (succ n)"
        `shouldBe` pure (tyNormalVec (app (var "succ") (var "n")))
    it "parses type applications (5)" $
      parseTypeExpr "Vec %(succ n)"
        `shouldBe` pure (tyPersVec (app (var "succ") (var "n")))
    it "parses code types (1)" $
      parseTypeExpr "&Int"
        `shouldBe` pure (tyCode tyInt)
    it "parses code types (2)" $
      parseTypeExpr "&(Vec n)"
        `shouldBe` pure (tyCode (tyNormalVec (var "n")))
    it "parses code types (3)" $
      parseTypeExpr "&Int -> Bool"
        `shouldBe` pure (tyNondepFun (tyCode tyInt) tyBool)
    it "parses code types (4)" $
      parseTypeExpr "&(Int -> Bool)"
        `shouldBe` pure (tyCode (tyNondepFun tyInt tyBool))
    it "parses refinement types" $
      parseTypeExpr "{n : Int | 0 <= n}"
        `shouldBe` pure (tyRefinement "n" tyInt (app (app (var "<=") (litInt 0)) (var "n")))
  describe "Parser.parseExpr (with code locations)" $ do
    it "parses integer literals" $
      parseExprWithLoc "42"
        `shouldBe` pure (exprLoc 0 2 $ Literal (LitInt 42))
    it "parses float literals" $
      parseExprWithLoc "5.7"
        `shouldBe` pure (exprLoc 0 3 $ Literal (LitFloat 5.7))
    it "parses vector literals" $
      parseExprWithLoc "[| 3; 14; 1592 |]"
        `shouldBe` pure (exprLoc 0 17 $ Literal (LitVec [3, 14, 1592]))
    it "parses matrix literals" $
      parseExprWithLoc "[# 3, 14; 159, 2; 653, 5 #]"
        `shouldBe` pure (exprLoc 0 27 $ Literal (LitMat [[3, 14], [159, 2], [653, 5]]))
    it "parses variables" $
      parseExprWithLoc "foo_bar"
        `shouldBe` pure (exprLoc 0 7 $ short "foo_bar")
    it "parses variables with module prefixes" $
      parseExprWithLoc "Foo.Bar.x"
        `shouldBe` pure (exprLoc 0 9 $ long ["Foo", "Bar"] "x")
    it "parses applications (1)" $
      parseExprWithLoc "x y"
        `shouldBe` pure (exprLoc 0 3 (App (exprLoc 0 1 (short "x")) Nothing (exprLoc 2 3 (short "y"))))
    it "parses applications (2)" $ do
      let e =
            exprLoc 0 5 $
              App
                (exprLoc 0 3 (App (exprLoc 0 1 (short "x")) Nothing (exprLoc 2 3 (short "y"))))
                Nothing
                (exprLoc 4 5 (short "z"))
      parseExprWithLoc "x y z"
        `shouldBe` pure e
    it "parses applications (3)" $ do
      let e =
            exprLoc 0 7 $
              App (exprLoc 0 1 (short "x")) Nothing $
                exprLoc 2 7 $
                  App (exprLoc 3 4 (short "y")) Nothing (exprLoc 5 6 (short "z"))
      parseExprWithLoc "x (y z)"
        `shouldBe` pure e
    it "parses brackets" $ do
      let e =
            exprLoc 0 8 $
              App (exprLoc 0 1 (short "f")) Nothing $
                exprLoc 2 8 $
                  Bracket $
                    exprLoc 3 8 $
                      App (exprLoc 4 5 (short "g")) Nothing (exprLoc 6 7 (short "x"))
      parseExprWithLoc "f &(g x)"
        `shouldBe` pure e
    it "parses lambda abstractions" $ do
      let ty =
            typLoc 9 26 $
              TyArrow
                Nothing
                (Just "n", typLoc 14 17 $ TyName "Int" [])
                (typLoc 22 26 $ TyName "Bool" [])
          e =
            exprLoc 0 34 $
              Lam Nothing Nothing ("x", ty) $
                exprLoc 31 34 $
                  App (exprLoc 31 32 (short "x")) Nothing (exprLoc 33 34 (short "y"))
      parseExprWithLoc "fun (x : (n : Int) -> Bool) -> x y"
        `shouldBe` pure e
    it "parses optional applications (1)" $
      parseExprWithLoc "x {y}"
        `shouldBe` pure (exprLoc 0 5 $ AppOptGiven (exprLoc 0 1 $ short "x") (exprLoc 3 4 $ short "y"))
    it "parses optional applications (2)" $ do
      let e =
            exprLoc 0 9 $
              AppOptGiven
                (exprLoc 0 5 $ AppOptGiven (exprLoc 0 1 $ short "x") (exprLoc 3 4 $ short "y"))
                (exprLoc 7 8 $ short "z")
      parseExprWithLoc "x {y} {z}"
        `shouldBe` pure e
  describe "parseBinds (without code locations)" $ do
    it "parses single, stage-1 normal binding" $
      parseBinds "val n = 42"
        `shouldBe` pure [Bind () (BindVal Stage1 "n" (BindValNormal (litInt 42)))]
    it "parses single, stage-0 normal binding" $
      parseBinds "val ~n = 42"
        `shouldBe` pure [Bind () (BindVal Stage0 "n" (BindValNormal (litInt 42)))]
    it "parses single, stage-0 external binding" $
      parseBinds "val ~foo : Int -> Bool external (builtin = \"bar\", surface = \"qux\")"
        `shouldBe` pure
          [ Bind () $
              BindVal Stage0 "foo" $
                BindValExternal
                  (tyNondepFun tyInt tyBool)
                  [("builtin", "bar"), ("surface", "qux")]
          ]
    it "parses single, stage-0 external binding (polymorphic)" $
      parseBinds "val ~app : forall 'a -> forall 'b -> ('a -> 'b) -> 'a -> 'b external (builtin = \"app\", surface = \"app\")"
        `shouldBe` pure
          [ Bind () $
              BindVal Stage0 "app" $
                BindValExternal
                  (tyForAll (TypeVar "a") $ tyForAll (TypeVar "b") $ tyNondepFun (tyNondepFun (tyVar "a") (tyVar "b")) (tyNondepFun (tyVar "a") (tyVar "b")))
                  [("builtin", "app"), ("surface", "app")]
          ]
