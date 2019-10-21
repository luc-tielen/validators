{-# LANGUAGE BangPatterns #-}

-- | This module defines the 'Validation' data type.
module Data.Validation
  ( Validation (..),
    toEither,
    fromEither,
  )
where

-- | A 'Validation' is very similar to 'Either' in that it contains a value of type /e/ or /a/.
-- In contrast to Either however, Validation accumulates all errors it comes
-- across in it's 'Applicative' instance.
--
-- A complete example where Validation is used can be found
-- <https://github.com/luc-tielen/validators.git here>.
data Validation err a
  = Failure !err
  | Success !a
  deriving (Eq, Show, Ord)

instance Functor (Validation err) where
  fmap _ (Failure err) = Failure err
  fmap f (Success a) = Success (f a)
  {-# INLINE fmap #-}

instance Semigroup err => Applicative (Validation err) where

  Failure e1 <*> Failure e2 = Failure $ e1 <> e2
  Failure e1 <*> _ = Failure e1
  _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success (f a)
  {-# INLINE (<*>) #-}

  pure = Success
  {-# INLINE pure #-}

-- | Conversion function from 'Validation' to 'Either'.
--
-- >>> toEither (Failure 1)
-- Left 1
-- >>> toEither (Success True)
-- Right True
toEither :: Validation err a -> Either err a
toEither v = case v of
  Failure e -> Left e
  Success a -> Right a
{-# INLINE toEither #-}

-- | Conversion function from 'Either' to 'Validation'.
--
-- >>> fromEither (Left 1)
-- Failure 1
-- >>> fromEither (Right True)
-- Success True
fromEither :: Either err a -> Validation err a
fromEither e = case e of
  Left a -> Failure a
  Right b -> Success b
{-# INLINE fromEither #-}
