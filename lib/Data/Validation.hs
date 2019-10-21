-- | This module defines the 'Validation' data type.
module Data.Validation
  ( Validation (..),
    toEither,
    fromEither,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.Bitraversable

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

instance Bifunctor Validation where
  bimap f _ (Failure e) = Failure (f e)
  bimap _ g (Success a) = Success (g a)
  {-# INLINE bimap #-}

instance Foldable (Validation err) where
  foldMap f (Success a) = f a
  foldMap _ _ = mempty
  {-# INLINE foldMap #-}

instance Bifoldable Validation where
  bifoldMap f _ (Failure e) = f e
  bifoldMap _ g (Success a) = g a
  {-# INLINE bifoldMap #-}

instance Traversable (Validation err) where
  traverse f (Success a) = Success <$> f a
  traverse _ (Failure e) = pure (Failure e)
  {-# INLINE traverse #-}

instance Bitraversable Validation where
  bitraverse _ g (Success a) = Success <$> g a
  bitraverse f _ (Failure e) = Failure <$> f e
  {-# INLINE bitraverse #-}

instance Semigroup err => Applicative (Validation err) where

  Failure e1 <*> Failure e2 = Failure (e1 <> e2)
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
