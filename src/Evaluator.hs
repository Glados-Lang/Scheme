-- Evaluator.hs
module Evaluator
  ( evalExpr,
    Value(..),
    Env
  ) where

import SchemeParser
import Data.Result
import qualified Data.Map as Map

type Env = Map.Map String Value

data Value
  = VNumber Integer
  | VBool Bool
  | VLambda [String] Expr Env
  deriving (Eq, Show)

-- | Checks if a function name corresponds to a built-in function.
isBuiltinFunction :: String -> Bool
isBuiltinFunction funcName = funcName `elem` ["+", "-", "*", "/", "=", ">", "<", ">=", "<=", "div", "eq?", "mod"]

-- | Evaluates an expression within a given environment.
evalExpr :: Env -> Expr -> Result String (Value, Env)
evalExpr env lexpr = case lexpr of
  Number n ->
    Ok (VNumber n, env)
  
  Bool b ->
    Ok (VBool b, env)
  
  Symbol s ->
    case Map.lookup s env of
      Just v  -> Ok (v, env)
      Nothing -> Err ("Undefined symbol: " ++ s)
  
  Define name valueExpr -> do
    case valueExpr of
      Lambda params body -> do
        -- Create a recursive closure environment immediately
        let recClosureEnv = Map.insert name functionValue env
            functionValue = VLambda params body recClosureEnv
        let env' = Map.insert name functionValue env
        Ok (functionValue, env')
      _ -> do
        (value, _) <- evalExpr env valueExpr
        let env' = Map.insert name value env
        Ok (value, env')
  
  Lambda params body ->
    Ok (VLambda params body env, env)
  
  If condExpr thenExpr elseExpr -> do
    (condValue, env') <- evalExpr env condExpr
    case condValue of
      VBool True  -> evalExpr env' thenExpr
      VBool False -> evalExpr env' elseExpr
      _           -> Err "Condition expression must evaluate to a boolean"
  
  Call funcExpr argExprs -> do
    case funcExpr of
      Symbol funcName | isBuiltinFunction funcName -> do
        (argValues, env') <- evalArguments env argExprs
        mapOk (\result -> (result, env')) (evalBuiltin funcName argValues)
      _ -> do
        (funcValue, env') <- evalExpr env funcExpr
        (argValues, env'') <- evalArguments env' argExprs
        value <- applyFunction funcValue argValues
        Ok (value, env'')

  -- Handle function application for List expressions
  List [] ->
    Err "Cannot evaluate an empty list"
  List (funcExpr:argExprs) ->
    evalExpr env (Call funcExpr argExprs)


-- | Evaluates a list of argument expressions.
evalArguments :: Env -> [Expr] -> Result String ([Value], Env)
evalArguments env [] = Ok ([], env)
evalArguments env (arg:args) = do
  (val, env') <- evalExpr env arg
  (vals, env'') <- evalArguments env' args
  return (val:vals, env'')

applyFunction :: Value -> [Value] -> Result String Value
applyFunction funcValue argValues = case funcValue of
  VLambda params body closureEnv ->
    if length params /= length argValues
      then Err "Incorrect number of arguments"
      else do
        let paramBindings = Map.fromList (zip params argValues)
        let localEnv = Map.union paramBindings closureEnv
        (value, _) <- evalExpr localEnv body
        Ok value
  _ -> Err "Attempted to call a non-function"


-- | Evaluates built-in functions.
evalBuiltin :: String -> [Value] -> Result String Value
evalBuiltin funcName args = case funcName of
  "+"  -> evalNumericOp (+) args
  "-"  -> evalNumericOp (-) args
  "*"  -> evalNumericOp (*) args
  "/"  -> evalNumericOp div args
  "="  -> evalNumericCompareOp (==) args
  ">"  -> evalNumericCompareOp (>) args
  "<"  -> evalNumericCompareOp (<) args
  ">=" -> evalNumericCompareOp (>=) args
  "<=" -> evalNumericCompareOp (<=) args
  "div" -> evalNumericOp div args
  "eq?" -> evalNumericCompareOp (==) args
  "mod" -> evalNumericOp mod args
  _    -> Err ("Unknown function: " ++ funcName)

-- | Performs a numeric operation on a list of values.
evalNumericOp :: (Integer -> Integer -> Integer) -> [Value] -> Result String Value
evalNumericOp op vals = do
  nums <- mapM extractNumber vals
  case nums of
    [] -> Err "No arguments provided"
    _  -> Ok (VNumber (foldl1 op nums))

-- | Performs a numeric comparison operation on two values.
evalNumericCompareOp :: (Integer -> Integer -> Bool) -> [Value] -> Result String Value
evalNumericCompareOp op [VNumber x, VNumber y] = Ok (VBool (op x y))
evalNumericCompareOp _ _ = Err "Expected two numeric arguments"

-- | Extracts an integer from a 'Value' or returns an error if it's not a number.
extractNumber :: Value -> Result String Integer
extractNumber (VNumber n) = Ok n
extractNumber _           = Err "Expected a number"
