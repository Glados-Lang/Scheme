-- test/EvaluatorSpec.hs
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module EvaluatorSpec (main, spec) where

import Data.Evaluator
import Data.SchemeParser (Expr (..))
import Data.Result
import qualified Data.Map as Map
import Test.Hspec
import Control.Monad (foldM)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Evaluator" $ do
    it "evaluates numbers" $ do
      evalExpr Map.empty (Number 42) `shouldBe` Ok (VNumber 42, Map.empty)

    it "evaluates booleans" $ do
      evalExpr Map.empty (Bool True) `shouldBe` Ok (VBool True, Map.empty)
      evalExpr Map.empty (Bool False) `shouldBe` Ok (VBool False, Map.empty)

    it "evaluates simple arithmetic operations" $ do
      let expr = Call (Symbol "+") [Number 2, Number 3]
      evalExpr Map.empty expr `shouldBe` Ok (VNumber 5, Map.empty)

    it "evaluates nested arithmetic operations" $ do
      let expr = Call (Symbol "+") [Number 2, Call (Symbol "*") [Number 3, Number 4]]
      evalExpr Map.empty expr `shouldBe` Ok (VNumber 14, Map.empty)

    it "handles undefined symbols" $ do
      evalExpr Map.empty (Symbol "x") `shouldBe` Err "Undefined symbol: x"

    it "evaluates define expressions" $ do
      let expr = Define "x" (Number 42)
      evalExpr Map.empty expr `shouldBe` Ok (VNumber 42, Map.fromList [("x", VNumber 42)])

    it "evaluates symbol lookup after define" $ do
      let exprs = [Define "x" (Number 42), Symbol "x"]
      let evalResult = evalSequence exprs
      evalResult `shouldBe` Ok (VNumber 42)

    it "evaluates lambda expressions" $ do
      let expr = Lambda ["x"] (Call (Symbol "+") [Symbol "x", Number 1])
      evalExpr Map.empty expr `shouldSatisfy` isOk

    it "evaluates function application" $ do
      let exprs = [ Define "inc" (Lambda ["x"] (Call (Symbol "+") [Symbol "x", Number 1])),
                    Call (Symbol "inc") [Number 5]
                  ]
      let evalResult = evalSequence exprs
      evalResult `shouldBe` Ok (VNumber 6)

    it "evaluates if expressions" $ do
      let expr = If (Bool True) (Number 1) (Number 2)
      evalExpr Map.empty expr `shouldBe` Ok (VNumber 1, Map.empty)
      let expr2 = If (Bool False) (Number 1) (Number 2)
      evalExpr Map.empty expr2 `shouldBe` Ok (VNumber 2, Map.empty)

    it "evaluates variable redefinition" $ do
      let exprs = [ Define "x" (Number 1),
                    Define "x" (Number 2),
                    Symbol "x"
                  ]
      let evalResult = evalSequence exprs
      evalResult `shouldBe` Ok (VNumber 2)

    it "handles variable shadowing in function scope" $ do
      let exprs = [ Define "x" (Number 10),
                    Define "f" (Lambda ["x"] (Call (Symbol "+") [Symbol "x", Number 5])),
                    Call (Symbol "f") [Number 3]
                  ]
      let evalResult = evalSequence exprs
      evalResult `shouldBe` Ok (VNumber 8)

    it "errors on incorrect argument types" $ do
      let expr = Call (Symbol "+") [Bool True, Number 1]
      evalExpr Map.empty expr `shouldBe` Err "Expected a number"

    it "errors on incorrect number of arguments" $ do
      let expr = Call (Symbol "+") [Number 1]
      evalExpr Map.empty expr `shouldBe` Ok (VNumber 1, Map.empty) -- Assuming '+' can be unary
      let expr2 = Call (Symbol "+") []
      evalExpr Map.empty expr2 `shouldBe` Err "No arguments provided"

    it "evaluates nested function applications" $ do
      let exprs = [ Define "add" (Lambda ["a", "b"] (Call (Symbol "+") [Symbol "a", Symbol "b"])),
                    Define "double" (Lambda ["x"] (Call (Symbol "add") [Symbol "x", Symbol "x"])),
                    Call (Symbol "double") [Number 5]
                  ]
      let evalResult = evalSequence exprs
      evalResult `shouldBe` Ok (VNumber 10)

    it "evaluates if expressions with non-boolean conditions" $ do
      let expr = If (Number 1) (Number 42) (Number 0)
      evalExpr Map.empty expr `shouldBe` Err "Condition expression must evaluate to a boolean"

    it "errors on calling non-functions" $ do
      let expr = Call (Number 42) [Number 1]
      evalExpr Map.empty expr `shouldBe` Err "Attempted to call a non-function"

    it "evaluates recursive functions" $ do
      let exprs = [ Define "factorial"
                      (Lambda ["n"]
                        (If (Call (Symbol "=") [Symbol "n", Number 0])
                            (Number 1)
                            (Call (Symbol "*")
                                  [ Symbol "n",
                                    Call (Symbol "factorial")
                                         [Call (Symbol "-") [Symbol "n", Number 1]]
                                  ]))),
                    Call (Symbol "factorial") [Number 5]
                  ]
      let evalResult = evalSequence exprs
      evalResult `shouldBe` Ok (VNumber 120)
    
    it "evaluates variable shadowing correctly" $ do
      let exprs = [ Define "x" (Number 10),
                    Define "f" (Lambda ["x"] (Call (Symbol "+") [Symbol "x", Number 5])),
                    Call (Symbol "f") [Number 3],
                    Symbol "x"
                  ]
      let evalResult = evalSequenceWithEnv exprs
      evalResult `shouldBe` Ok (VNumber 10)

    it "errors on incorrect number of arguments in function call" $ do
      let exprs = [ Define "add" (Lambda ["a", "b"] (Call (Symbol "+") [Symbol "a", Symbol "b"])),
                    Call (Symbol "add") [Number 1]
                  ]
      let evalResult = evalSequence exprs
      evalResult `shouldBe` Err "Incorrect number of arguments"

-- Helper function to evaluate a sequence of expressions, returning the value of the last one.
evalSequence :: [Expr] -> Data.Result.Result String Value
evalSequence exprs = do
  let initialEnv = Map.empty
  (value, _) <- foldM evalAndUpdate (VBool False, initialEnv) exprs
  return value
  where
    evalAndUpdate (_, env) expr = do
      (newValue, newEnv) <- evalExpr env expr
      return (newValue, newEnv)


evalSequenceWithEnv :: [Expr] -> Data.Result.Result String Value
evalSequenceWithEnv exprs = do
  let initialEnv = Map.empty
  (_, env) <- foldM evalAndUpdate (VBool False, initialEnv) exprs
  case Map.lookup "x" env of
    Just (VNumber n) -> Ok (VNumber n)
    _                -> Err "Variable 'x' not found"
  where
    evalAndUpdate (_, env) expr = do
      (newValue, newEnv) <- evalExpr env expr
      return (newValue, newEnv)
