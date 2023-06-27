{-# language TupleSections #-}
{-# language MultiWayIf #-}
module GPSB2 where

import Data.Char ( digitToInt, toUpper )
import Data.List ( tails, findIndex )
import Rec
{-
basement :: [Int] -> Int
basement xs = fst $ accu st alg (fromList xs) 0
  where
      st NilF _ = NilF
      st (ConsF x ys) s = ConsF x (ys, s+1)

      alg NilF s = (s, 0)
      alg (ConsF x (ix, acc)) s = if x + acc < 0 then (s, x + acc) else (ix, x + acc)

bouncingBalls :: Double -> Double -> Int -> Double
bouncingBalls fstHeight sndHeight n = hylo alg coalg fstHeight
    where 
        bounciness = (sndHeight / fstHeight)
        coalg x = ConsF x (x * bounciness)
        alg NilF = 0
        alg (ConsF x xs) = 2*x + xs

bowling :: String -> Int
bowling = undefined
  where
    charToScore 'X' = 10
    charToScore '/' = 10
    charToScore '-' = 0
    charToScore c = digitToInt c

camelCase :: String -> String
camelCase xs = accu st alg (fromList xs) ("", False)
  where
    st NilF s = NilF
    st (ConsF x xs) (s1, s2) = ConsF x (xs, (
            if | s2 -> [toUpper x]
               | elem x " -_" -> ""
               | otherwise -> [x]
          , if | elem x " -_" -> True
               | otherwise -> False
          ))
    alg NilF s = fst s
    alg (ConsF x xs) s = fst s <> xs

coinSums :: Int -> (Int, [Int])
coinSums n = hylo alg coalg n
  where
    coins = [25, 10, 5, 1]
    alg 0 = NilF
    alg x = ConsF 

cutVector :: [Int] -> ([Int], [Int])
cutVector = undefined

diceGame :: Int -> Int -> Double
diceGame = undefined

findPair :: [Int] -> Int -> (Int, Int)
findPair = undefined

fizzBuzz :: Int -> [String]
fizzBuzz = undefined

fuelCost :: [Int] -> Int
fuelCost = undefined

gcd :: Int -> Int -> Int
gcd = undefined

indicesSubStr :: String -> String -> [Int]
indicesSubStr = undefined

leaders :: [Int] -> [Int]
leaders = undefined

luhn :: [Int] -> Int
luhn = undefined

masterMind :: String -> String -> (Int, Int)
masterMind = undefined

middleChar :: String -> String
middleChar = undefined

pairedDigits :: String -> Int
pairedDigits = undefined

shoppingList :: [Double] -> [Double] -> Double
shoppingList = undefined

snowDay :: Int -> Double -> Double -> Double -> Double
snowDay = undefined

solveBool :: String -> Bool
solveBool = undefined

spinWords :: String -> String
spinWords = undefined

squareDigits :: Int -> String
squareDigits = reverse . concat . toList . ana coalg
  where
    coalg x = case x of
                0 -> NilF
                y -> ConsF (show $ (y `mod` 10) ^ 2) (y `div` 10)

subsCipher :: String -> String -> String -> String
subsCipher = undefined

twitter :: String -> String
twitter xs = accu st alg (fromList xs) 0
  where
    st NilF _ = NilF
    st (ConsF x xs) s = ConsF x (xs, s+1)
    alg NilF s | s == 0 = "You didn't type anything"
               | s > 140 = "Too many characters"
               | otherwise = "Your tweet has " <> show s <> " characters"
    alg (ConsF x xs) s = xs

vecDistance :: [Double] -> [Double] -> Double
vecDistance xs ys = sqrt $ cata alg (fromList xs) ys
  where
    alg NilF ys = 0
    alg (ConsF x xs) ys = (x - head ys)^2 + xs (tail ys)
-}