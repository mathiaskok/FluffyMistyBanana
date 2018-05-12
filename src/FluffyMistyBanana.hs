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

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry m (EitherRight (Right a)) = EitherRight $ Right $ m a
  furry m (EitherRight (Left b)) = EitherRight $ Left $ b

-- This is clearly identical to the monad typeclass. bind and return are not used as this would make the problems trivial.
class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' map mist = banana (unicorn . map) mist

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  unicorn a = [a]
  banana _ [] = []
  banana m (x : xs) = (m x) ++ (banana m xs)


-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  unicorn = Just
  banana _ Nothing = Nothing
  banana m (Just a) = m a

-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  unicorn a = \_ -> a
  banana m fa = \t -> m (fa t) t

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  unicorn = EitherLeft . Left
  banana _ (EitherLeft (Right b)) = EitherLeft $ Right b
  banana m (EitherLeft (Left a)) = m a

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  unicorn = EitherRight . Right
  banana _ (EitherRight (Left b)) = EitherRight $ Left b
  banana m (EitherRight (Right a)) = m a

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple mist applier = 
  banana (\fa -> 
    banana (\a -> unicorn $ fa a) mist)
    applier

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy = error "todo"

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = error "todo"

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"