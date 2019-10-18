module Data.Validator
  ( Validator
  , validate
  , check
  ) where

import Data.Validation


data Result e = Ok | Err e
  deriving (Eq, Show)

instance Semigroup e => Semigroup (Result e) where
  Ok <> Ok = Ok
  Err e1 <> Err e2 = Err $ e1 <> e2
  Err e1 <> _ = Err e1
  _ <> Err e2 = Err e2

instance Monoid e => Monoid (Result e) where
  mempty = Ok


newtype Validator e a = Validator (a -> Result e)

instance Semigroup e => Semigroup (Validator e a) where
  Validator v1 <> Validator v2 = Validator $ v1 <> v2

instance Monoid e => Monoid (Validator e a) where
  mempty = Validator mempty

-- | Runs the validators on a single subject
validate :: Validator e a -> a -> Validation e a
validate (Validator f) a = case f a of
  Err e -> Failure e
  Ok -> Success a

-- TODO rename to ifTrue / mkValidator
check :: (a -> Bool) -> e -> Validator e a
check p e = Validator $ \a -> if p a then Ok else Err e

