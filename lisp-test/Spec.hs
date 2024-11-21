{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Text (Text, pack)
import SchemeParser (Expr (..), parseExpr)
import Test.Hspec
import Test.QuickCheck

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

      genSymbol = elements ["x", "y", "z", "+", "-", "*", "/"]
      genDefine n = Define <$> genSymbol <*> exprGen n
      genLambda n = Lambda <$> listOf genSymbol <*> exprGen n
      genIf n = If <$> exprGen n <*> exprGen n <*> exprGen n
      genCall n = Call <$> exprGen n <*> listOf (exprGen n)



main :: IO ()
main = hspec $ do
  describe "SchemeParser" $ do
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
       in parsed == Right expr

-- Helper to render Expr back to Text for testing
renderExpr :: Expr -> Text
renderExpr expr = case expr of
  Number n -> pack $ show n
  Bool True -> "#t"
  Bool False -> "#f"
  Symbol s -> pack s
  List es -> "(" <> renderExprs es <> ")"
  Define name value -> "(define " <> pack name <> " " <> renderExpr value <> ")"
  Lambda params body ->
    "(lambda (" <> pack (unwords params) <> ") " <> renderExpr body <> ")"
  If cond t f ->
    "(if " <> renderExpr cond <> " " <> renderExpr t <> " " <> renderExpr f <> ")"
  Call func args ->
    "(" <> renderExpr func <> " " <> renderExprs args <> ")"

renderExprs :: [Expr] -> Text
renderExprs exprs = pack $ unwords $ map (show . renderExpr) exprs
