
module Data.Validation
  ( Valid
  , Validation(..)
  , Validator
  , fromValid
  , check
  , validate
  , runValidation
  ) where

import Prelude

newtype Valid a = Valid { fromValid :: a }
  deriving (Eq, Show)

instance Functor Valid where
  fmap f (Valid a) = Valid (f a)

instance Applicative Valid where
  pure = Valid

  Valid f <*> Valid a = Valid $ f a

data Validation e a
  = Failure e
  | Success a

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Semigroup e => Applicative (Validation e) where
  pure = Success

  Failure e1 <*> Failure e2 = Failure $ e1 <> e2
  Failure e1 <*> _ = Failure e1
  _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success $ f a

data Result e = Ok | Err e
  deriving (Eq, Show)

instance Semigroup e => Semigroup (Result e) where
  Ok <> Ok = Ok
  (Err e1) <> (Err e2) = Err (e1 <> e2)
  (Err e1) <> _ = Err e1
  _ <> Err e2 = Err e2

-- TODO newtype?
type Validator e a = a -> Result e

-- TODO rename to ifTrue / mkValidator
check :: (a -> Bool) -> e -> Validator e a
check p e a =
  if p a
    then Ok
    else Err e

validate :: Validator e a -> a -> Validation e a
validate f a = case f a of
  Err e -> Failure e
  Ok -> Success a

runValidation :: Validation e a -> Either e (Valid a)
runValidation (Failure e) = Left e
runValidation (Success a) = Right $ Valid a

