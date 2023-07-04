-- Some instances and utilities
--
-- Given the standard type classes for functors, applicative functors and monads:
--

-----

-- class Functor f where
--   fmap :: (a -> b) -> f a -> f b
--
-- class Functor f => Applicative f where
--   pure :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--
-- class Applicative f => Monad f where
--   return :: a -> f a
--   (>>=) :: f a -> (a -> f b) -> f b
--

-----

-- Give instances for all three classes for the following data types:
--
-- data Tree a = Leaf a | Node (Tree a) (Tree a)
--
-- data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf
--

-----

-- Foldable and traversable
--
-- Also give instances for the Foldable and Traversable classes, whenever possible:
--
-- class Foldable t where
--   foldMap :: Monoid m => (a -> m) -> t a -> m
--
-- class Traversable t where
--   traverse :: Applicative f => (a -> f b) -> t a -> f (t b)

-----

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap f (Leaf a)   = Leaf (f a)
  fmap f (Node l r) = Node (fmap f l) (fmap f r)

instance Applicative Tree where
  pure = Leaf

  Leaf f   <*> t = fmap f t
  Node l r <*> t = Node (l <*> t) (r <*> t)

instance Monad Tree where
  return = pure

  Leaf a   >>= f = f a
  Node l r >>= f = Node (l >>= f) (r >>= f)

instance Foldable Tree where
  foldMap f (Leaf a)   = f a
  foldMap f (Node l r) = foldMap f l `mappend` foldMap f r

instance Traversable Tree where
   traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
   traverse f (Leaf a)   = Leaf <$> f a
   traverse f (Node l r) = Node <$> traverse f l <*> traverse f r


tree :: Tree Int
tree = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))

-- functor
func1 = (+1)
main1 = putStrLn (show (fmap func1 tree))

-- applicative
func2 = Leaf (+1)
main2 = putStrLn (show (func2 <*> tree))

-- monad
func3 n = Leaf (n + 1)
main3   = putStrLn (show (tree >>= func3))

-- foldable
data IntM = IntM Int deriving Show

instance Semigroup IntM where
  (<>) (IntM a) (IntM b) = IntM (a + b)

instance Monoid IntM where
  mempty = IntM 0

main7 = putStrLn (show (foldMap IntM tree))

-- traversable
func8 a = [a]
main8   = putStrLn (show (traverse func8 tree))


data RoseTree a = RoseNode a [RoseTree a] | RoseLeaf deriving Show

instance Functor RoseTree where
  fmap f RoseLeaf        = RoseLeaf
  fmap f (RoseNode a ts) = RoseNode (f a) (fmap (fmap f) ts)

instance Applicative RoseTree where
  pure a = RoseNode a []

  RoseLeaf      <*> RoseLeaf      = RoseLeaf
  RoseNode f fs <*> RoseNode a ts = RoseNode (f a) (zipWith (<*>) fs ts)

instance Monad RoseTree where
  return = pure

  RoseLeaf      >>= _ = RoseLeaf
  RoseNode a ts >>= f = case f a of
    RoseLeaf      -> RoseLeaf
    RoseNode b ns -> RoseNode b (ns ++ fmap (>>= f) ts)

instance Foldable RoseTree where
  foldMap _ RoseLeaf            = mempty
  foldMap f (RoseNode a ts) = f a `mappend` (foldMap (foldMap f) ts)

instance Traversable RoseTree where
  traverse :: Applicative f => (a -> f b) -> RoseTree a -> f (RoseTree b)
  traverse f RoseLeaf        = pure RoseLeaf
  traverse f (RoseNode a ts) = RoseNode <$> f a <*> sequenceA (fmap (traverse f) ts)



roseTree = RoseNode 1 [(RoseNode 2 []), (RoseNode 3 [RoseNode 4 []])]

-- functor
func4 = (+1)
main4 = putStrLn (show (fmap func4 roseTree))

-- applicative
func5 = RoseNode (+1) [(RoseNode (+2) []), (RoseNode (+3) [RoseNode (+4) []])]
main5 = putStrLn (show (func5 <*> roseTree))

-- monad
func6 a = RoseNode (a * 2) [RoseNode a []]
main6 = putStrLn (show (roseTree >>= func6))

-- foldable
main9 = putStrLn (show (foldMap IntM roseTree))


-- traversable
func10 a = [a]
main10   = putStrLn (show (traverse func10 roseTree))
