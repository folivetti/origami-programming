{-# language RankNTypes, DeriveFunctor #-}
module Rec where

data ListF a b = NilF | ConsF a b deriving Functor
data IListF a b = INilF | IConsF Int a b deriving Functor
data NatF a = ZeroF | SuccF a deriving Functor
data StreamF a b = StreamF a b deriving Functor
data TreeF a b = LeafF | NodeF b a b deriving Functor

newtype Fix f = Fix {unfix :: f (Fix f)}

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

data Cofree f a = a :< f (Cofree f a)
data Free f a = Ret a | Op (f (Free f a))

extract :: Cofree f a -> a
extract (x :< _) = x

unOp :: Free f a -> f (Free f a)
unOp (Op x) = x
unOp _ = error "partial function unOp called on Ret"

cata :: Functor f => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix
-- stack everything then unstack

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg
-- stack and unstack and insert into structure OR stack everything and then unstack into a list

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo alg coalg = alg . fmap (cata alg . ana coalg) . coalg
-- stack then unstack

para :: Functor f => (f (Fix f, a) -> a) -> Fix f -> a
para alg = alg . fmap (id &&& para alg) . unfix
  where (f &&& g) x = (f x, g x)
-- same as cata

mutu :: Functor f => (f (a, b) -> a) -> (f (a, b) -> b) -> (Fix f -> a, Fix f -> b)
mutu alg1 alg2 = (fst . cata alg, snd . cata alg)
  where alg x = (alg1 x, alg2 x)
-- same as cata

apo :: Functor f => (a -> f (Either (Fix f) a)) -> a -> Fix f
apo coalg = Fix . fmap (id ||| apo coalg) . coalg
  where 
      (f ||| g) (Left x)  = f x
      (f ||| g) (Right y) = g y
-- same as ana (?)

accu :: Functor f => (forall x. f x -> p -> f (x, p)) -> (f a -> p -> a) -> Fix f -> p -> a
accu st alg (Fix t) p = alg (fmap (uncurry (accu st alg)) (st t p)) p
-- stack, stack, then unstack

-- histo, futu, chrono, dyno may have a different stacking behavior
histo :: Functor f => (f (Cofree f a) -> a) -> Fix f -> a
histo alg = extract . cata (\x -> alg x :< x)

futu :: Functor f => (a -> f (Free f a)) -> a -> Fix f
futu coalg = ana coalg' . Ret
  where
    coalg' (Ret a) = coalg a
    coalg' (Op k) = k

chrono :: Functor f => (f (Cofree f b) -> b) -> (a -> f (Free f a)) -> a -> b
chrono alg coalg = extract . hylo alg' coalg' . Ret
  where
    alg' x = alg x :< x
    coalg' (Ret a) = coalg a
    coalg' (Op k) = k

-- * Conversion functions

fromList :: [a] -> Fix (ListF a)
fromList [] = Fix NilF
fromList (x:xs) = Fix (ConsF x (fromList xs))

toList :: Fix (ListF a) -> [a]
toList (Fix NilF) = []
toList (Fix (ConsF x xs)) = x : toList xs

fromIList :: [a] -> Fix (IListF a)
fromIList xs = go (zip [0..] xs)
  where
    go [] = Fix INilF
    go ((ix,x):xs) = Fix (IConsF ix x (go xs))

toIList :: Fix (IListF a) -> [a]
toIList (Fix INilF) = []
toIList (Fix (IConsF ix x xs)) = x : toIList xs

icons :: a -> Fix (IListF a) -> Fix (IListF a)
icons x xs = fromIList (x : toIList xs)

stream2list :: StreamF a [a] -> [a]
stream2list (StreamF x y) = x : y

toNat :: Int -> Fix NatF
toNat 0 = Fix ZeroF
toNat n = Fix (SuccF (toNat (n-1)))

fromNat :: Fix NatF -> Int
fromNat (Fix ZeroF) = 0
fromNat (Fix (SuccF x)) = 1 + fromNat x
