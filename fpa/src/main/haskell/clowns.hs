{-# language PatternSynonyms        #-}


import Data.Char


data Expr = Val Int | Add Expr Expr

-- non-tail recursive evaluator

eval1 :: Expr -> Int
eval1 (Val i)     = i
eval1 (Add e1 e2) = eval1 e1 + eval1 e2


-- tail recursive evaluator employing a stack

type Stack = [Either Expr Int]

eval2 :: Expr -> Int
eval2 e = load e []

load :: Expr -> Stack -> Int
load (Val i) stk     = unload i stk
load (Add e1 e2) stk = load e1 (Left e2 : stk)

unload :: Int -> Stack -> Int
unload v []                = v
unload v1 (Left e2 : stk)  = load e2 (Right v1 : stk)
unload v2 (Right v1 : stk) = unload (v1 + v2) stk


-- generic data component kit

data K a   x = K a                -- constant
data I     x = I x                -- identity
data S l r x = L (l x) | R (r x)  -- sum type aka either
data P l r x = P (l x) (r x)      -- product type aka tuple


-- eg. unit generically defined as a constant

type One = K ()

-- or eg. the option type generically defined as a sum type

type Option = S One I

none :: Option ()
none   = L (K ())

some :: a -> Option a
some x = R (I x)

instance Show a => Show (Option a) where
  show (L (K ())) = "None"
  show (R (I x))  = "Some(" ++ show x ++ ")"


-- the kit is functorial

instance Functor (K a) where
  fmap f (K a) = K a

instance Functor I where
  fmap f (I s) = I (f s)

instance (Functor l, Functor r) => Functor (S l r) where
  fmap f (L l) = L (fmap f l)
  fmap f (R r) = R (fmap f r)

instance (Functor l, Functor r) => Functor (P l r) where
  fmap f (P l r) = P (fmap f l) (fmap f r)


-- and our reconstructed option is functorial without further ado

main1 :: IO ()
main1 = print (fmap isDigit (some '1'))


-- the expr branching structure is readily described by a polynomial
type ExprP = S (K Int) (P I I)

pattern ValP i     = L (K i)
pattern AddP e1 e2 = R (P (I e1) (I e2))


-- we would like now to establish the isomorphism: Expr ∼= ExprP Expr
-- which we do via a type level fix point to tie the knot inductively
data Mu p = In (p (Mu p))

type Expr2 = Mu ExprP

pattern ValM i     = In (ValP i)
pattern AddM e1 e2 = In (AddP e1 e2)


-- this lets us define a fold like recursion operator as a catamorphism - see doc/img/cata.png
cata :: Functor p => (p v -> v) -> Mu p -> v
cata phi (In p) = phi (fmap (cata phi) p)


-- with a subsequent evaluator for in terms of that catamorphism
eval3 :: Expr2 -> Int
eval3 =
  cata phi where
    phi (ValP v)   = v
    phi (AddP m n) = m + n


-- we shall see how to turn a cata into a first- order tail-recursion whenever p is polynomial
-- we shall do this by dissecting p, with ‘clown’ elements left and ‘joker’s on the right
-- to this end, we need polynomial bifunctors, which are just functors, but in two directions
-- let's construct a data component kit for that with two generic type parameters `x` and `y`
data K2  a  x y = K2 a
data Fst    x y = Fst x
data Snd    x y = Snd y
data S2 a b x y = L2 (a x y) | R2 (b x y)
data P2 a b x y = P2 (a x y) (b x y)

-- with eg. again, unit generically defined as a constant
type One2 = K2 ()


class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

instance Bifunctor (K2 a) where
  bimap f g (K2 a) = K2 a

instance Bifunctor Fst where
  bimap f g (Fst x) = Fst (f x)

instance Bifunctor Snd where
  bimap f g (Snd y) = Snd (g y)

instance (Bifunctor p, Bifunctor q) => Bifunctor (S2 p q) where
  bimap f g (L2 p) = L2 (bimap f g p)
  bimap f g (R2 q) = R2 (bimap f g q)

instance (Bifunctor p, Bifunctor q) => Bifunctor (P2 p q) where
  bimap f g (P2 p q) = P2 (bimap f g p) (bimap f g q)


-- but.. but.. nothing is missing - we need non-constructable zero
data Zero

