-- | This module defines the 'Validator' data type and helper functions
--   for creating various validators.
module Data.Validator
  ( Validator,
    validate,
    assert,
    refute,
  )
where

import Data.Validation

-- | Helper data type resembling the result of a 'Validator' assertion.
--
-- There are only 2 possible results:
-- 1. 'Ok': the validator assertion succeeded.
-- 2. 'Err': the validator assertion failed.
--
-- Only used internally in the 'Validator' type to keep track of accumulated errors.
data Result err = Ok | Err err
  deriving (Eq, Show)

instance Semigroup err => Semigroup (Result err) where
  Ok <> Ok = Ok
  Err e1 <> Err e2 = Err $ e1 <> e2
  Err e1 <> _ = Err e1
  _ <> Err e2 = Err e2

instance Monoid err => Monoid (Result err) where
  mempty = Ok

-- | Datatype for checking if a validation holds for a subject.
--
-- - __subject__ can be any data type for which assertions need to be checked.
-- - __err__ can be any type representing an error, but it will only be possible to
-- combine validators if the error type has a Semigroup instance.
--
-- Execute a validator by passing it to the 'validate' function.
--
-- A Validator is both a 'Semigroup' and a 'Monoid', making it possible to combine
-- smaller validators into larger validators. A combined validator will accumulate
-- errors from all of it's sub-validators.
newtype Validator err subject = Validator (subject -> Result err)

instance Semigroup err => Semigroup (Validator err subject) where
  Validator v1 <> Validator v2 = Validator $ v1 <> v2

instance Monoid err => Monoid (Validator err subject) where
  mempty = Validator mempty

-- | Runs a validator on a subject.
--
-- The result is a 'Validation' containing all accumulated errors,
-- or the subject wrapped in a 'Success' value.
validate :: Validator err subject -> subject -> Validation err subject
validate (Validator f) a = case f a of
  Err e -> Failure e
  Ok -> Success a

-- | Creates a validator that will return an error if the given predicate doesn't hold.
--
-- Usage:
--
-- >>> let validator = assert (> 10) ["too small"]
-- >>> validate validator 11
-- Success 11
--
-- >>> validate validator 1
-- Failure ["too small"]
assert :: (subject -> Bool) -> err -> Validator err subject
assert p err = Validator $ \subject -> if p subject then Ok else Err err

-- | Creates a validator that will return an error if the given predicate holds.
--
-- Usage:
--
-- >>> let validator = refute (> 10) ["too big"]
-- >>> validate validator 11
-- Failure ["too big"]
--
-- >>> validate validator 1
-- Success 1
refute :: (subject -> Bool) -> err -> Validator err subject
refute p = assert (not . p)
