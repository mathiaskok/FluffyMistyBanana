module FluffyMistyBanana(module FluffyMistyBanana) where

-- This is clearly identical to the functor typeclass. fmap is not used as this would make the problems trivial.
class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry _ [] = []
  furry m (x : xs) = (m x) : (furry m xs)

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry _ Nothing = Nothing
  furry m (Just a) = Just $ m a


-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry m f = \t -> m $ f t

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry m (EitherLeft (Left a)) = EitherLeft $ Left $ m a
  furry m (EitherLeft (Right b)) = EitherLeft $ Right $ b

  