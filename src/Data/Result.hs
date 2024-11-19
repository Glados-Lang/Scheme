module Data.Result
  ( Result (..),
    mapOk,
    mapErr,
    andThen,
    unwrap,
    unwrapOr,
    isOk,
    isErr,
  )
where

data Result e a = Ok a | Err e
  deriving (Ord, Read, Show)

instance Functor (Result e) where
  fmap = mapOk

instance Applicative (Result e) where
  pure = Ok
  (Ok f) <*> (Ok x) = Ok (f x)
  (Err e) <*> _ = Err e
  _ <*> (Err e) = Err e

instance Monad (Result e) where
  return = pure
  (>>=) = andThen

instance (Eq o, Eq e) => Eq (Result o e) where
  (Ok x) == (Ok y) = x == y
  (Err x) == (Err y) = x == y
  _ == _ = False

mapOk :: (a -> b) -> Result e a -> Result e b
mapOk f (Ok x) = Ok (f x)
mapOk _ (Err e) = Err e

mapErr :: (e -> f) -> Result e a -> Result f a
mapErr _ (Ok x) = Ok x
mapErr f (Err e) = Err (f e)

andThen :: Result e a -> (a -> Result e b) -> Result e b
andThen (Err e) _ = Err e
andThen (Ok x) f = f x

unwrap :: Result e a -> a
unwrap (Ok x) = x
unwrap (Err _) = error "Called unwrap on an Err value"

unwrapOr :: a -> Result e a -> a
unwrapOr _ (Ok x) = x
unwrapOr defaultValue (Err _) = defaultValue

isOk :: Result e a -> Bool
isOk (Ok _) = True
isOk (Err _) = False

isErr :: Result e a -> Bool
isErr = not . isOk
