module Data.Validation (Validation (..)) where

-- | Used for combining multiple validators together
data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success $ f a

instance Semigroup e => Applicative (Validation e) where

  pure = Success

  Failure e1 <*> Failure e2 = Failure $ e1 <> e2
  Failure e1 <*> _ = Failure e1
  _ <*> Failure e2 = Failure e2
  Success f <*> Success a = Success $ f a
