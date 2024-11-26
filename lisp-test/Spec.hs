{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Text (Text, pack)
import SchemeParser (Expr (..), boolean, call, define, ifExpr, lambda, list, number, parseExpr, sc, symbolExpr)
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

-- For QuickCheck
instance Arbitrary Expr where
  arbitrary = sized exprGen
    where
      exprGen 0 =
        oneof
          [ Number <$> arbitrary,
            Bool <$> arbitrary,
            Symbol <$> genSymbol
          ]
      exprGen n =
        oneof
          [ Number <$> arbitrary,
            Bool <$> arbitrary,
            Symbol <$> genSymbol,
            List <$> listOf (exprGen (n `div` 2)),
            genDefine (n - 1),
            genLambda (n - 1),
            genIf (n - 1),
            genCall (n - 1)
          ]

      genSymbol = elements $ map (: []) "abcdefghijklmnopqrstuvwxyz*/+-" ++ ["->", "=>", "<=", ">=", "map", "add", "copy"]
      genDefine n = Define <$> genSymbol <*> exprGen n
      genLambda n = Lambda <$> listOf genSymbol <*> exprGen n
      genIf n = If <$> exprGen n <*> exprGen n <*> exprGen n
      genCall n = Call <$> exprGen n <*> listOf (exprGen n)

main :: IO ()
main = hspec $ do
  describe "SchemeParser" $ do
    describe "Basic Parsers" $ do
      it "sc handles various whitespace and comments" $ do
        runParser sc "" "   \n\t  " `shouldBe` Right ()
        runParser sc "" "; comment\n  " `shouldBe` Right ()

      it "number parser" $ do
        runParser number "" "42" `shouldBe` Right (Number 42)
        runParser number "" "-42" `shouldBe` Right (Number (-42))
        runParser number "" "0" `shouldBe` Right (Number 0)
        runParser number "" "- 33" `shouldNotBe` Right (Number (-33))

      it "boolean parser" $ do
        runParser boolean "" "#t" `shouldBe` Right (Bool True)
        runParser boolean "" "#f" `shouldBe` Right (Bool False)

      it "symbolExpr parser" $ do
        runParser symbolExpr "" "abc" `shouldBe` Right (Symbol "abc")
        runParser symbolExpr "" "+" `shouldBe` Right (Symbol "+")
        runParser symbolExpr "" "..." `shouldBe` Right (Symbol "...")
        runParser symbolExpr "" "a1" `shouldBe` Right (Symbol "a1")
        runParser symbolExpr "" "->salut" `shouldBe` Right (Symbol "->salut")
        runParser symbolExpr "" "." `shouldNotBe` Right (Symbol ".")
        runParser symbolExpr "" "...." `shouldNotBe` Right (Symbol "...")
        runParser symbolExpr "" "@-oui" `shouldNotBe` Right (Symbol "@-oui")
        runParser symbolExpr "" "a-b-c" `shouldBe` Right (Symbol "a-b-c")
        runParser symbolExpr "" "a b c" `shouldBe` Right (Symbol "a")
        runParser symbolExpr "" "mon_add?" `shouldBe` Right (Symbol "mon_add?")

      it "list parser" $ do
        runParser list "" "(1 2 3)" `shouldBe` Right (List [Number 1, Number 2, Number 3])
        runParser list "" "()" `shouldBe` Right (List [])
        runParser list "" "(#t #f)" `shouldBe` Right (List [Bool True, Bool False])

    describe "Complex Parsers" $ do
      it "define with direct value" $ do
        runParser define "" "(define x 42)" `shouldBe` Right (Define "x" (Number 42))

      it "define with function" $ do
        runParser define "" "(define foo 42)" `shouldBe` Right (Define "foo" (Number 42))
        runParser define "" "(define add\n (lambda (x y)\n    (+ x y)))"
          `shouldBe` Right (Define "add" (Lambda ["x", "y"] (Call (Symbol "+") [Symbol "x", Symbol "y"])))
        runParser define "" "(define (add a b) (+ a b))"
          `shouldBe` Right (Define "add" (Lambda ["a", "b"] (Call (Symbol "+") [Symbol "a", Symbol "b"])))
        runParser define "" "(define (f x) (if (> x 0) x (- x)))"
          `shouldBe` Right
            ( Define
                "f"
                ( Lambda
                    ["x"]
                    ( If
                        (Call (Symbol ">") [Symbol "x", Number 0])
                        (Symbol "x")
                        (Call (Symbol "-") [Symbol "x"])
                    )
                )
            )
        runParser define "" "(define (true) #t)" `shouldBe` Right (Define "true" (Lambda [] (Bool True)))

      it "lambda expressions" $ do
        runParser lambda "" "(lambda () 42)" `shouldBe` Right (Lambda [] (Number 42))
        runParser lambda "" "(lambda (x y) (+ x y))"
          `shouldBe` Right (Lambda ["x", "y"] (Call (Symbol "+") [Symbol "x", Symbol "y"]))

      it "if expressions" $ do
        runParser ifExpr "" "(if #t 1 2)"
          `shouldBe` Right (If (Bool True) (Number 1) (Number 2))
        runParser ifExpr "" "(if (= x 0) #t #f)"
          `shouldBe` Right (If (Call (Symbol "=") [Symbol "x", Number 0]) (Bool True) (Bool False))

      it "function calls" $ do
        runParser call "" "(+ 1 2)"
          `shouldBe` Right (Call (Symbol "+") [Number 1, Number 2])
        runParser call "" "(f)" `shouldBe` Right (Call (Symbol "f") [])

    describe "Integration Tests" $ do
      it "handles nested expressions" $ do
        parseExpr "(define (add x y) (+ x y))"
          `shouldBe` Right
            ( Define
                "add"
                ( Lambda
                    ["x", "y"]
                    ( Call
                        (Symbol "+")
                        [Symbol "x", Symbol "y"]
                    )
                )
            )
        parseExpr "(define (matrix_add m1 m2) (map (lambda (a b) (map + a b)) m1 m2))"
          `shouldBe` Right
            ( Define
                "matrix_add"
                ( Lambda
                    ["m1", "m2"]
                    ( Call
                        (Symbol "map")
                        [ Lambda
                            ["a", "b"]
                            ( Call
                                (Symbol "map")
                                [Symbol "+", Symbol "a", Symbol "b"]
                            ),
                          Symbol "m1",
                          Symbol "m2"
                        ]
                    )
                )
            )

      it "handles complex define with nested lambda" $ do
        parseExpr "(define compose (lambda (f g) (lambda (x) (f (g x)))))"
          `shouldBe` Right
            ( Define
                "compose"
                ( Lambda
                    ["f", "g"]
                    ( Lambda
                        ["x"]
                        ( Call
                            (Symbol "f")
                            [Call (Symbol "g") [Symbol "x"]]
                        )
                    )
                )
            )

    describe "Expresion parsing" $ do
      it "parses numbers" $ do
        parseExpr "42" `shouldBe` Right (Number 42)
        parseExpr "0" `shouldBe` Right (Number 0)
        parseExpr "1234567890" `shouldBe` Right (Number 1234567890)
        parseExpr "12345678901234567890" `shouldBe` Right (Number 12345678901234567890)

      it "parses booleans" $ do
        parseExpr "#t" `shouldBe` Right (Bool True)
        parseExpr "#f" `shouldBe` Right (Bool False)

      it "parses symbols" $ do
        parseExpr "x" `shouldBe` Right (Symbol "x")
        parseExpr "&" `shouldBe` Right (Symbol "&")
        parseExpr "+" `shouldBe` Right (Symbol "+")

      it "parses lists" $ do
        parseExpr "(1 2 3)" `shouldBe` Right (List [Number 1, Number 2, Number 3])

      it "parses define expressions" $ do
        parseExpr "(define x 42)"
          `shouldBe` Right (Define "x" (Number 42))

      it "parses lambda expressions" $ do
        parseExpr "(lambda (x) x)"
          `shouldBe` Right (Lambda ["x"] (Symbol "x"))

      it "parses if expressions" $ do
        parseExpr "(if #t 1 0)"
          `shouldBe` Right (If (Bool True) (Number 1) (Number 0))

      it "parses nested expressions" $ do
        parseExpr "(define add (lambda (x y) (+ x y)))"
          `shouldBe` Right
            ( Define
                "add"
                ( Lambda
                    ["x", "y"]
                    (Call (Symbol "+") [Symbol "x", Symbol "y"])
                )
            )

      it "property: can parse any generated Expr" $ property $ \expr ->
        let rendered = renderExpr expr
            parsed = parseExpr rendered
         in do
              case parsed of
                Right parsedExpr -> rendered `shouldBe` renderExpr parsedExpr
                Left err -> expectationFailure $ show err
              parsed `shouldBe` Right expr

-- Helper func
renderExpr :: Expr -> Text
renderExpr expr = case expr of
  Number n -> pack $ show n
  Bool True -> "#t"
  Bool False -> "#f"
  Symbol s -> pack s
  List es -> "(" <> renderExprs es <> ")"
  Define name value -> "(define " <> pack name <> " " <> renderExpr value <> ")"
  Lambda params body ->
    "(lambda (" <> pack (unwords params) <> ")" <> renderExpr body <> ")"
  If cond t f ->
    "(if " <> renderExpr cond <> " " <> renderExpr t <> " " <> renderExpr f <> ")"
  Call func args ->
    "(" <> renderExpr func <> " " <> renderExprs args <> ")"

renderExprs :: [Expr] -> Text
renderExprs [] = ""
renderExprs [x] = renderExpr x
renderExprs (x : xs) = renderExpr x <> (if null xs then "" else " " <> renderExprs xs)
