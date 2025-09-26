{-# language RankNTypes, DeriveFunctor #-}
module Rec where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

data ListF a b = NilF | ConsF a b deriving Functor
data IListF a b = INilF | IConsF Int a b deriving Functor
data SetF a b = SNilF | SConsF a b deriving Functor
data MapF k v b = MNilF | MConsF k v b deriving Functor
data NatF a = ZeroF | SuccF a deriving Functor
data StreamF a b = StreamF a b deriving Functor
data TreeF a b = LeafF | NodeF b a b deriving Functor
data Delay a b = Value a | Delayed b deriving Functor

newtype Fix f = Fix {unfix :: f (Fix f)}

type Algebra f a = f a -> a
type CoAlgebra f a = a -> f a

data Cofree f a = a :< f (Cofree f a)
data Free f a = Ret a | Op (f (Free f a))

extract :: Cofree f a -> a
extract (x :< _) = x

nextCF :: Cofree (ListF a1) a2 -> Cofree (ListF a1) a2
nextCF cf@(_ :< NilF) = cf
nextCF (_ :< ConsF _ tbl) = tbl

nextElem :: Cofree (ListF a1) a2 -> Maybe a1
nextElem (_ :< NilF) = Nothing 
nextElem (_ :< ConsF x _) = Just x 

lenCF :: Cofree (ListF a) b -> Int
lenCF (_ :< NilF) = 1
lenCF cf = 1 + lenCF (nextCF cf)

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

accuHisto :: Functor f => (forall x. f x -> p -> f (x, p)) -> (f (Cofree f a) -> p -> a) -> Fix f -> p -> a
accuHisto st alg t p = extract $ accu st alg' t p
  where
    alg' x s = alg x s :< x

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

fromSet :: Ord a => S.Set a -> Fix (SetF a)
fromSet set = go (S.toList set)
  where
    go [] = Fix SNilF
    go (x:xs) = Fix (SConsF x (go xs))

fromMap :: Ord k => M.Map k v -> Fix (MapF k v)
fromMap m = go (M.toList m)
  where
    go [] = Fix MNilF
    go ((k,v):xs) = Fix (MConsF k v (go xs))

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
