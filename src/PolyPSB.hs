{-# language TupleSections #-}
{-# language MultiWayIf #-}
module GPSB2 where

import Data.Char ( digitToInt, toUpper, toLower )
import Data.List ( tails, findIndex )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad (join)
import Rec
import Debug.Trace

areaOfRectangle :: (Double, Double) -> Double
areaOfRectangle (w, h) = w * h

centimetersToMeters :: Double -> Double
centimetersToMeters cm = cm / 100.0

countTrue :: [Bool] -> Int
countTrue xs = cata alg (fromList xs)
  where
    alg NilF = 0
    alg (ConsF x xs) = if x then succ xs else xs

filterBounds :: Ord a => S.Set a -> a -> a -> S.Set a
filterBounds set lower upper = cata alg (fromSet set) where
  alg SNilF = S.empty
  alg (SConsF x acc)
    | x >= lower && x <= upper = S.insert x acc
    | otherwise = acc

firstIndexOfTrue :: [a] -> (a -> Bool) -> Int
firstIndexOfTrue xs p = length $ takeWhile (not . p) $ xs

firstIndexOfTrueCata :: [a] -> (a -> Bool) -> Int
firstIndexOfTrueCata xs p = cata alg (fromIList xs) where
  alg INilF = 0 -- can actually be any value since p is guaranteed to return true for some element
  alg (IConsF i x acc) = if p x then i else acc

getValsOfKey :: [M.Map String Int] -> String -> [Int]
getValsOfKey maps key = cata alg (fromIList maps) where
  alg INilF = []
  alg (IConsF i x acc) = ((unsafeLookup x key) : acc)

maxAppliedFunction :: Int -> (Int -> Int) -> Int
maxAppliedFunction n f = hylo alg coalg 0 where

  coalg seed
      | seed == n = NilF 
      | otherwise = ConsF seed (succ seed)
  alg NilF = pred n
  alg (ConsF x acc) = if f x > f acc then x else acc

minKey :: Ord a => M.Map a Int -> a
minKey m = cata alg (fromMap m) where
  alg MNilF = head (M.keys m)
  alg (MConsF k v acc) = if v < (unsafeLookup m acc) then k else acc


seSymmetricDifference :: Ord a => S.Set a -> S.Set a -> S.Set a
seSymmetricDifference s1 s2 = S.union (S.difference s1 s2) (S.difference s2 s1)

setsWithElement :: S.Set (S.Set Int) -> Int -> S.Set (S.Set Int)
setsWithElement set element = cata alg (fromSet set) where
  alg SNilF = S.empty
  alg (SConsF x acc)
    | element `S.member` x = S.insert x acc
    | otherwise = acc

simpleEncryption :: String -> (Char -> Char) -> String
simpleEncryption str f = cata alg (fromIList str) where
  alg INilF = []
  alg (IConsF i x acc) = f x : acc

sum2Vals :: M.Map String Int -> String -> String -> Int
sum2Vals m k1 k2 = ((unsafeLookup m k2) + (unsafeLookup m k1))


sum2ValsPolymorphic :: Ord k => M.Map k Int -> k -> k -> Int
sum2ValsPolymorphic m k1 k2 = ((unsafeLookup m k2) + (unsafeLookup m k1))

sum2D :: [[Int]] -> Int
sum2D xss = sum (join xss)

sumVectorVals :: [String] -> M.Map String Int -> Int
sumVectorVals keys m = cata alg (fromIList keys) where
  alg INilF = 0
  alg (IConsF i x acc) = unsafeLookup m x + acc

timeSheet :: [(String, Int)] -> String -> Int
timeSheet entries name = cata alg (fromIList entries) where
  alg INilF = 0
  alg (IConsF i (n, h) acc) = if n == name then h + acc else acc

-- helper function
unsafeLookup :: Ord k => M.Map k v -> k -> v
unsafeLookup m k = case M.lookup k m of
  Just v -> v
  Nothing -> error "Key not found"