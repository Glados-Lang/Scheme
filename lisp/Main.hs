-- Main.hs
module Main 
  ( main
  ) where

import SchemeParser (Expr, expr)
import Evaluator (evalExpr, Value(..), Env)
import Data.Result (Result(..))
import qualified Data.Text.IO as TIO
import qualified Data.Map as Map
import Control.Monad (foldM)
import Text.Megaparsec (parse, errorBundlePretty)
import Control.Applicative (many)
import System.Exit (exitWith, ExitCode(..))


main :: IO ()
main = do
  input <- TIO.getContents

  -- Parse the input into expressions.
  let parseResult = parse (many expr) "" input
  case parseResult of
    Left err -> do
      putStrLn (errorBundlePretty err)
      exitWith (ExitFailure 84)

    Right exprs -> do
      let initialEnv :: Env
          initialEnv = Map.empty
      let evalResult = foldM evalAndUpdateEnv (VBool False, initialEnv) exprs
      case evalResult of
        Err errMsg -> do
          putStrLn ("Error: " ++ errMsg)
          exitWith (ExitFailure 84)

        Ok (value, _) ->
          printValue value


-- | Helper function to evaluate an expression and update the environment.
evalAndUpdateEnv :: (Value, Env) -> Expr -> Result String (Value, Env)
evalAndUpdateEnv (_, env) lexpr = evalExpr env lexpr

-- | Prints the evaluated value.
printValue :: Value -> IO ()
printValue (VNumber n) = print n
printValue (VBool True) = putStrLn "#t"
printValue (VBool False) = putStrLn "#f"
printValue (VLambda _ _ _) = putStrLn "<function>"
