import Prelude hiding (lookup)
import Data.Map
import Data.Maybe

-- part 1

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
func10 a = [a,a]
main10   = putStrLn (show (traverse func10 roseTree))


-- part 2

lookupAll :: Ord k => [k] -> Map k v -> Maybe [v]
lookupAll ks m = sequence $ ks >>= (\s -> return $ lookup s m)

lookupSome :: Ord k => [k] -> Map k v -> [v]
lookupSome ks m = ks >>= (\s -> maybeToList $ lookup s m)

testMap = fromList [(0, "zero"), (1, "one"), (2, "two")]
main11 = putStrLn (show (lookupAll  [0, 2] testMap))
main12 = putStrLn (show (lookupSome [0, 2] testMap))
