{-# language PatternSynonyms        #-}
{-# language FunctionalDependencies #-}
{-# language UndecidableInstances   #-}
{-# language LambdaCase             #-}

import Data.Char
import Data.Bifunctor
import Unsafe.Coerce


-- the use case is to employ a simple recursive ADT
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

data K1 a   x = K1 a                 -- constant
data I1     x = I1 x                 -- identity
data S1 l r x = L1 (l x) | R1 (r x)  -- sum type aka either
data P1 l r x = P1 (l x) (r x)       -- product type aka tuple


-- eg. unit generically defined as a constant

type One = K1 ()

-- or eg. the option type generically defined as a sum type

type Option = S1 One I1

none :: Option ()
none   = L1 (K1 ())

some :: a -> Option a
some x = R1 (I1 x)

instance Show a => Show (Option a) where
  show (L1 (K1 ())) = "None"
  show (R1 (I1 x))  = "Some(" ++ show x ++ ")"


-- the kit is functorial

instance Functor (K1 a) where
  fmap f (K1 a) = K1 a

instance Functor I1 where
  fmap f (I1 s) = I1 (f s)

instance (Functor l, Functor r) => Functor (S1 l r) where
  fmap f (L1 l) = L1 (fmap f l)
  fmap f (R1 r) = R1 (fmap f r)

instance (Functor l, Functor r) => Functor (P1 l r) where
  fmap f (P1 l r) = P1 (fmap f l) (fmap f r)


-- and our reconstructed option is functorial without further ado

main1 :: IO ()
main1 = print (fmap isDigit (some '1'))


-- the expr branching structure is readily described by a polynomial
type ExprP = S1 (K1 Int) (P1 I1 I1)

pattern ValP i     = L1 (K1 i)
pattern AddP e1 e2 = R1 (P1 (I1 e1) (I1 e2))


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

magic :: Zero -> a
magic z = z `seq` error "we never get this far"

inflate1 :: Functor p => p Zero -> p x
inflate1 = fmap magic

inflate2 :: Functor p => p Zero -> p x
inflate2 = unsafeCoerce

type Zero1 = K1 Zero
type Zero2 = K2 Zero

-- with that we can dissect left clowns and right jokers
data Clown p c j = Clown (p c)

instance Functor f => Bifunctor (Clown f) where
  bimap f g (Clown p) = Clown (fmap f p)

data Joker p c j = Joker (p j)

instance Functor f => Bifunctor (Joker f) where
  bimap f g (Joker p)= Joker (fmap g p)


-- given a left functor and right bifunctor we define a dissection to have an implication on the right
-- with that we can define instances in terms of our left and right component kits of which the product
-- type proves to require the most horrific encoding
class (Functor p, Bifunctor q) => D p q | p -> q where
  right :: Either (p j) (q c j, c) -> Either (j, q c j) (p c)

-- for constants, we jump from far left to far right in one go; we cannot be in the middle, so we refute that case
instance D (K1 a) (K2 Zero) where
  right (Left (K1 a)) = Right (K1 a)
  right (Right (K2 zero, _)) = magic zero

-- for identities, ie. single element, we can step into or step out
instance D I1 (K2 ()) where
  right (Left (I1 j)) = Left (j, K2 ())
  right (Right (K2 (), c)) = Right (I1 c)

-- for sums, we use the instance for the branch appropriate, strip tags beforehand and replace them afterwards
instance (D p p', D q q') => D (S1 p q) (S2 p' q') where
  right (Left (L1 p)) = (bimap (fmap L2) L1) (right (Left p))
  right (Left (R1 q)) = (bimap (fmap R2) R1) (right (Left q))
  right (Right (L2 p, c)) = (bimap (fmap L2) L1) (right (Right (p, c)))
  right (Right (R2 q, c)) = (bimap (fmap R2) R1) (right (Right (q, c)))

-- for products, we start at the left of the first element and end at the right of the second, but we also need
-- to make things join up in the middle; ie. when we reach the far right of the first component, we must continue
-- from the far left of the second
instance (D p p', D q q') => D (P1 p q) (S2 (P2 p' (Joker q)) (P2 (Clown p) q')) where
  right = rightProd

rightProd
  :: (D p p', D q q')
  => Either ((P1 p q) j) ((S2 (P2 p' (Joker q)) (P2 (Clown p) q')) c j, c)
  -> Either (j, (S2 (P2 p' (Joker q)) (P2 (Clown p) q')) c j) ((P1 p q) c)
rightProd = \case
  Left (P1 p q) ->
    kP q (right (Left p))
  Right (L2 (P2 p (Joker q)), c) ->
    kP q (right (Right (p, c)))
  Right (R2 (P2 (Clown p) q), c) ->
    kQ p (right (Right (q, c)))

kP
  :: (D p p', D q q')
  => q j
  -> Either (j, p' c j) (p c)
  -> Either (j, (S2 (P2 p' (Joker q)) (P2 (Clown p) q') c j)) ((P1 p q) c)
kP q = \case
  Left (j, p') ->
    Left (j, L2 (P2 p' (Joker q)))
  Right p' ->
    kQ p' (right (Left q))

kQ
  :: (D p p', D q q')
  => p c
  -> Either (j, q' c j) (q c)
  -> Either (j, (S2 (P2 p' (Joker q)) (P2 (Clown p) q')) c j) ((P1 p q) c)
kQ p = \case
  Left (j, q') ->
    Left (j, R2 (P2 (Clown p) q'))
  Right q' ->
    Right (P1 p q')


-- we put the right operation straight to work. given a dissect p, we can make its fmap operation tail recursive
-- here, the jokers are the source elements and the clowns are the target elements, thus providing us with a
-- tail-recursive catamorphism for phi in term of mu
tmap :: forall a b q p. D p q => (a -> b) -> p a -> p b
tmap f ps =
  continue (right (Left ps))

  where
    continue :: Either (a, q b a) (p b) -> p b
    continue = \case
      Left (a, q) ->
        continue (right (Right (q, f a)))
      Right p ->
        p

-- ff we want to define the catamorphism via dissection, we could just replace fmap by tmap in the definition of cata
-- but that would be cheating!  the point, after all, is to turn a higher-order recursive program into a tail-recursive
-- machine. we need some kind of stack; s uppose we have a p-algebra  `p v -> v`, and we’re traversing a μ p depth-first
-- left-to-right, in order to compute a ‘value’ in v.  at any given stage, we’ll be processing a given node, in the
-- middle of traversing her mother, in the middle of traversing her grandmother, and so on in a maternal line back to
-- the root.  we’ll have visited all the nodes left of this line and thus have computed `v`s for them;  right of the
-- line, each element will contain a `μ p` waiting for her turn.  correspondingly, our stack is a list of dissections:
-- `[D p v (μ p)]`

-- we start, ready to load a tree, with an empty stack
cataD :: D p q => (p v -> v) -> Mu p -> v
cataD phi t = loadD phi t []

-- to load an element, we unpack her container of sub-elements and step in from the far left
loadD :: D p q => (p v -> v) -> Mu p -> [q v (Mu p)] -> v
loadD phi (In p) = nextD phi (right (Left p))

-- if after a step, we arrive at another sub-element, we load her, suspending the traversal of her mother by
-- pushing the dissection on the stack
nextD
  :: D p q
  => (p v -> v)
  -> Either (Mu p, q v (Mu p)) (p v)
  -> [q v (Mu p)]
  -> v
nextD phi x s =
  case x of
    Left (p, q) ->
      loadD phi p (q : s)
    Right p ->
      unloadD phi (phi p) s

-- alternatively, our step might have taken us to the far right of an element, in which case we have all her sub-
-- elements values:  we are ready to apply the `p v -> v` algebra to get her own value, and start unloading.  once we
-- have a sub element's value, we may resume the traversal of her mother, pushing the value into her place and moving on
unloadD :: D p q => (p v -> v) -> v -> [q v (Mu p)] -> v
unloadD phi v = \case
  [] ->
    v
  q : qs ->
    nextD phi (right (Right (q, v))) qs

-- on the other hand, if the stack is empty, then we’re holding the value for the root node
--
-- so we’re done <3
evalD :: Expr2 -> Int
evalD = cataD phi
  where
    phi (ValP n) = n
    phi (AddP n m) = n + m