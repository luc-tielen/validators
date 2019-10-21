{-# LANGUAGE FlexibleInstances #-}

-- | This module defines the 'Validator' data type and helper functions
--   for creating various validators.
module Data.Validator
  ( Validator,
    validate,
    assert,
    refute,
    ifNothing,
    ifLeft,
    HasSize (..),
    ifEmpty,
    minSize,
    maxSize,
    IsOnlyWhiteSpace (..),
    ifBlank,
  )
where

import Data.Either (isRight)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Validation

-- | Helper data type resembling the result of a 'Validator' assertion.
--
-- There are only 2 possible results:
-- 1. Ok: the validator assertion succeeded.
-- 2. Err: the validator assertion failed.
--
-- Only used internally in the 'Validator' type to keep track of accumulated errors.
data Result err = Ok | Err err
  deriving (Eq, Show)

instance Semigroup err => Semigroup (Result err) where
  Ok <> Ok = Ok
  Err e1 <> Err e2 = Err $ e1 <> e2
  Err e1 <> _ = Err e1
  _ <> Err e2 = Err e2
  {-# INLINE (<>) #-}

instance Monoid err => Monoid (Result err) where
  mempty = Ok
  {-# INLINE mempty #-}

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
  {-# INLINE (<>) #-}

instance Monoid err => Monoid (Validator err subject) where
  mempty = Validator mempty
  {-# INLINE mempty #-}

-- | Runs a validator on a subject.
--
-- The result is a 'Validation' containing all accumulated errors,
-- or the subject wrapped in a 'Success' value.
validate :: Validator err subject -> subject -> Validation err subject
validate (Validator f) a = case f a of
  Err e -> Failure e
  Ok -> Success a
{-# INLINE validate #-}

-- | Creates a validator that will return an error if the given predicate doesn't hold.
--
-- Since any predicate can be provided for checking if the subject satisfies
-- certain conditions, this can be used to build your own custom validators.
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
{-# INLINE assert #-}

-- | Creates a validator that will return an error if the given predicate holds.
--
-- Since any predicate can be provided for checking if the subject satisfies
-- certain conditions, this can be used to build your own custom validators.
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
{-# INLINE refute #-}

-- | Returns an error if a 'Maybe' is 'Nothing'.
--
-- Usage:
--
-- >>> let validator = ifNothing ["Found nothing."]
-- >>> validate validator Nothing
-- Failure ["Found nothing."]
--
-- >>> validate validator (Just "Bob")
-- Success (Just "Bob")
ifNothing :: err -> Validator err (Maybe a)
ifNothing = assert isJust
{-# INLINE ifNothing #-}

-- | Returns an error if an 'Either' contains a 'Left'.
--
-- Usage:
--
-- >>> let validator = ifLeft ["Found left."]
-- >>> validate validator (Left 123)
-- Failure ["Found left."]
--
-- >>> validate validator (Right 456)
-- Success (Right 456)
ifLeft :: err -> Validator err (Either a b)
ifLeft = assert isRight
{-# INLINE ifLeft #-}

-- | Helper typeclass for checking if a value is empty.
-- Used in the 'ifEmpty' validator.
class HasSize a where
  size :: a -> Int

  isEmpty :: a -> Bool
  isEmpty = (== 0) . size

instance HasSize [a] where
  size = length
  {-# INLINE size #-}

  isEmpty = null
  {-# INLINE isEmpty #-}

instance HasSize (Map k v) where
  size = Map.size
  {-# INLINE size #-}

  isEmpty = Map.null
  {-# INLINE isEmpty #-}

instance HasSize (Set a) where
  size = Set.size
  {-# INLINE size #-}

  isEmpty = Set.null
  {-# INLINE isEmpty #-}

instance HasSize (Seq a) where
  size = Seq.length
  {-# INLINE size #-}

  isEmpty = Seq.null
  {-# INLINE isEmpty #-}

-- | Returns an error if the function returns an "empty" value.
--
-- Usage:
--
-- >>> let validator = ifEmpty ["Empty."]
-- >>> validate validator []
-- Failure ["Empty."]
--
-- >>> validate validator [1, 2, 3]
-- Success [1,2,3]
-- >>> validate validator (Map.fromList [('a', 1), ('b', 2)])
-- Success (fromList [('a',1),('b',2)])
ifEmpty :: HasSize subject => err -> Validator err subject
ifEmpty = refute isEmpty
{-# INLINE ifEmpty #-}

-- | Returns an error if the value has a size smaller than required.
--
-- Usage:
--
-- >>> let validator = minSize 3 ["Too small."]
-- >>> validate validator []
-- Failure ["Too small."]
-- >>> validate validator [1, 2]
-- Failure ["Too small."]
-- >>> validate validator [1, 2, 3]
-- Success [1,2,3]
minSize :: HasSize subject => Int -> err -> Validator err subject
minSize x = refute ((< x) . size)
{-# INLINE minSize #-}

-- | Returns an error if the value has a size smaller than required.
--
-- Usage:
--
-- >>> let validator = maxSize 3 ["Too big."]
-- >>> validate validator [1, 2, 3, 4]
-- Failure ["Too big."]
-- >>> validate validator [1, 2, 3]
-- Success [1,2,3]
maxSize :: HasSize subject => Int -> err -> Validator err subject
maxSize x = refute ((> x) . size)
{-# INLINE maxSize #-}

-- | Helper typeclass for checking if a value contains only whitespace characters.
-- Used in the 'ifBlank validator.
class IsOnlyWhiteSpace a where
  isOnlyWhiteSpace :: a -> Bool

instance IsOnlyWhiteSpace String where
  isOnlyWhiteSpace = null . words
  {-# INLINE isOnlyWhiteSpace #-}

instance IsOnlyWhiteSpace TL.Text where
  isOnlyWhiteSpace = null . TL.words
  {-# INLINE isOnlyWhiteSpace #-}

instance IsOnlyWhiteSpace T.Text where
  isOnlyWhiteSpace = null . T.words
  {-# INLINE isOnlyWhiteSpace #-}

-- | Returns an error if the function returns a value containing only whitespace.
--
-- Usage:
--
-- >>> let validator = ifBlank ["Only whitespace."]
-- >>> validate validator "   \t \n \r "
-- Failure ["Only whitespace."]
--
-- >>> validate validator "not empty"
-- Success "not empty"
ifBlank :: IsOnlyWhiteSpace subject => err -> Validator err subject
ifBlank = refute isOnlyWhiteSpace
{-# INLINE ifBlank #-}

