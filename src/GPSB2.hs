{-# language TupleSections #-}
{-# language MultiWayIf #-}
module GPSB2 where

import Data.Char ( digitToInt, toUpper )
import Data.List ( tails, findIndex )
import Rec
import Debug.Trace ( trace )

basement :: [Int] -> Maybe Int
basement xs = cata alg2 $ cata alg (fromIList xs)
  where
    alg2 INilF = Nothing
    alg2 (IConsF ix x xs) = if x < 0 then Just ix else xs

    alg INilF = Fix INilF
    alg (IConsF ix x xs) = case xs of
                              Fix INilF -> icons x xs
                              Fix (IConsF iy y ys) -> icons (x+y) xs 

bowling :: String -> Int
bowling = histo alg . fromList
  where
    alg :: ListF Char (Cofree (ListF Char) Int) -> Int
    alg NilF = 0 
    alg (ConsF x table) = case x of
                            'X' -> 10 + if lenCof table <= 2
                                        then getBonusX table
                                        else extract table + getBonusX table
                            '/' -> 10 + if lenCof table <= 1 
                                          then getBonusS table
                                          else extract table + getBonusS table
                            c   -> charToScore c + extract table - correctS (charToScore c) table

    lenCof (_ :< NilF) = 0
    lenCof (_ :< (ConsF _ table)) = 1 + lenCof table

    getBonusX :: Cofree (ListF Char) Int -> Int
    getBonusX (score :< NilF) = 0
    getBonusX (score :< (ConsF '/' table)) = 10
    getBonusX (score :< (ConsF x table)) = charToScore x + getBonusS table - correctS (charToScore x) table

    getBonusS (score :< NilF) = 0
    getBonusS (score :< (ConsF x table)) = charToScore x

    correctS p (score :< NilF) = 0
    correctS p (score :< (ConsF '/' table)) = p
    correctS p _ = 0

    charToScore :: Char -> Int
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
{-
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

-}

twitter :: String -> String
twitter xs = cata alg (fromIList xs)
  where
    alg INilF = "You didn't type anything"
    alg (IConsF ix x xs) = if | xs == "You didn't type anything" -> if ix > 139 then xs else "Your tweet has " <> show (ix + 1) <> " characters"
                              | otherwise -> xs

vecDistance :: [Double] -> [Double] -> Double
vecDistance xs ys = sqrt $ cata alg (fromList xs) ys
  where
    alg NilF ys = 0
    alg (ConsF x xs) ys = if null ys then xs [] else ((x - head ys)^2 + xs (tail ys))
