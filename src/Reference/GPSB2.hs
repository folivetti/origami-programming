-----------------------------------------------------------------------------
-- |
-- Module      :  Reference.GPSB 
-- Copyright   :  (c) Fabricio Olivetti 
-- License     :  BSD3
-- Maintainer  :  fabricio.olivetti@gmail.com
-- Stability   :  experimental
-- Portability :  TupleSections
--
-- Reference solution for the GPSB2 - general program synthesis benchmark 2 problems 
-- This module still needs validation.
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
module Reference.GPSB2 where

import Data.Char (digitToInt, toUpper, toLower)
import Data.List ( tails, findIndex )
import qualified Data.Map.Strict as M
import Rec

-- basement
basement :: [Int] -> Maybe Int
basement = findIndex (<0) . map sum . tails

-- bouncing balls *
bouncingBalls :: Double -> Double -> Int -> Double
bouncingBalls h1 h2 n = h1 * (1 + 2 * b * (if b == 1 then b^(n-1) else (b^n - 1)/(b - 1)))
    where
        b = h2 / h1

bowling :: String -> Int
bowling card = score card 10
  where
    charToScore 'X' = 10
    charToScore '/' = 10
    charToScore '-' = 0
    charToScore c = digitToInt c

    score :: String -> Int -> Int
    score [] _ = 0
    score _ 0  = 0
    score [_] _ = 0
    score ('X':cs) n = if cs !! 1 == '/'
                          then 20 + score cs (n - 1)
                          else 10 + charToScore (head cs) + charToScore (cs !! 1) + score cs (n - 1)
    score (_:'/':cs) n = 10 + charToScore (head cs) + score cs (n-1)
    score (c1:c2:cs) n = charToScore c1 + charToScore c2 + score cs (n-1)


{-
X/ = 20
X23 = 10 + 2 + 3 + 2 + 3
-}
camelCase :: String -> String
camelCase = splitStr
  where
    capitalize "" = ""
    capitalize (x:xs) = toUpper x : map toLower xs 

    splitStr "" = ""
    splitStr xs = let ys = takeWhile (/= '-') xs 
                      zs = dropWhile (/= '-') xs
                   in if null zs 
                         then capitalize ys 
                         else capitalize ys <> splitStr (tail zs)


coinSums :: Int -> [Int]
coinSums x = reverse $ go x [] coins
  where
    coins = [25, 10, 5, 1]
    go _ res [] = res
    go y amounts (a:as) = let (q, r) = y `divMod` a in go r (q : amounts) as

cutVector :: [Int] -> ([Int], [Int])
cutVector = go []
  where
    diff (xs, ys) = abs (sum xs - sum ys)
    go xs [] = (xs, [])
    go xs ys
      | diff (xs, ys) < diff minGo = (xs, ys)
      | otherwise = minGo
      where
        minGo = go (xs <> [head ys]) (tail ys)

diceGame :: Int -> Int -> Double
diceGame n m = 1.0 - (fromIntegral m + 1) / (2 * fromIntegral n)

findPair :: [Int] -> Int -> (Int, Int)
findPair xs n = go xs
  where
    go [] = (0, 0)
    go (x:xs) = if (n - x) `elem` xs
                  then (x, n - x)
                  else go xs

fizzBuzz :: Int -> [String]
fizzBuzz n = map fb [1 .. n]
  where
    fb x | x `mod` 3 == 0 && x `mod` 5 == 0 = "FizzBuzz"
         | x `mod` 3 == 0 = "Fizz"
         | x `mod` 5 == 0 = "Buzz"
         | otherwise = show x

fuelCost :: [Int] -> Int
fuelCost xs = sum [x `div` 3 - 2 | x <- xs]

gcd' :: Int -> Int -> Int
gcd' a 0 = a
gcd' 0 b = b
gcd' a b = gcd' b (a `mod` b)

indicesSubStr :: String -> String -> [Int]
indicesSubStr css target = go 0 css
  where
    go ix "" = []
    go ix xs = if take (length target) xs == target
                then ix : go (ix + 1) (tail xs)
                else go (ix + 1) (tail xs)

leaders :: [Int] -> [Int]
leaders = reverse . go []
  where
    go acc [] = acc
    go acc (x:xs) = if all (<=x) xs then go (x:acc) xs else go acc xs

luhn :: [Int] -> Int
luhn = go1
  where
    go1 [] = 0
    go1 (x:xs) = x + go2 xs
    go2 [] = 0
    go2 (x:xs) = let y = x*2 in if y >= 10 then (y-9) + go1 xs else y + go1 xs

masterMind :: String -> String -> (Int, Int)
masterMind code guess = (sum $ [1 | (x,y) <- zip code guess, x==y], sum $ [1 | x <- guess, elem x code])

middleChar :: String -> String
middleChar "" = ""
middleChar xs | even n = [xs !! (ix-1), xs !! ix]
              | odd  n = [xs !! ix]
  where
    n = length xs
    ix = n `div` 2

pairedDigits :: String -> Int
pairedDigits = go
  where
    go "" = 0
    go [c] = 0
    go (c1:c2:cs) | c1 == c2 = digitToInt c1 + go (c2:cs)
                  | otherwise = go (c2:cs)

shoppingList :: [Double] -> [Double] -> Double
shoppingList xs ys = sum $ zipWith (\a b -> a - a * b / 100) xs ys

snowDay :: Int -> Double -> Double -> Double -> Double
snowDay hours snow rate melt = go hours snow
  where
    go 0 s = s
    go n s = go (n-1) ((s + rate) * melt) -- (s * melt + rate * melt) * melt + rate * melt

solveBool :: String -> Bool
solveBool = go
  where
    go [] = False
    go [c] = if c == 't' then True else False
    go cs = let b = solve (take 3 cs) in go (b : drop 3 cs)
    solve "t|t" = 't'
    solve "t|f" = 't'
    solve "f|t" = 't'
    solve "t&t" = 't'
    solve _     = 'f'

spinWords :: String -> String
spinWords xs = unwords [ if length w >= 5 then reverse w else w | w <- words xs ]

squareDigits :: Int -> String
squareDigits 0 = "0"
squareDigits x = go x
  where
    go 0 = ""
    go n = go (n `div` 10) <> show ((n `mod` 10) ^ 2)

subsCipher :: String -> String -> String -> String
subsCipher xs ys code = [cipher M.! c | c <- code]
  where
    cipher = M.fromList (zip xs ys)

twitter :: String -> String
twitter "" = "You didn't type anything"
twitter xs | length xs > 140 = "Too many characters"
           | otherwise       = "Your tweet has " <> show (length xs) <> " characters"

vecDistance :: [Double] -> [Double] -> Double
vecDistance xs ys = sqrt $ sum $ zipWith (\x y -> (x - y)^2) xs ys
