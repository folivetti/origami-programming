{-# language TupleSections #-}
{-# language MultiWayIf #-}
module GPSB2 where

import Data.Char ( digitToInt, toUpper, toLower )
import Data.List ( tails, findIndex )
import Rec
import Debug.Trace ( trace )
import GPSB (xWordLines)

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
bowling = snd . histo alg . fromList . reverse
  where
    alg NilF = (0, 0) :: (Int, Int)
    alg (ConsF 'X' ((ix, acc) :< NilF)) = (ix + 1, acc + 10)
    alg (ConsF x ((ix, acc) :< NilF))   = (ix, acc + charToScore x)

    alg (ConsF x table) =
        let (ix, acc) = extract table
            (iy, _) = extract (nextCF table)
            bonusS = ix < 10 && case nextElem table of
                       Just 'X' -> True
                       Just '/' -> True
                       _ -> False
            bonusX = ix < 10 && case nextElem (nextCF table) of
                       Just 'X' -> True
                       c -> False
            score  = charToScore x - if x == '/' then prevScore else 0
            points = score + (if bonusS then score else 0) + (if bonusX then score else 0)
            prevScore = maybe 0 charToScore (nextElem table)
         in if x == 'X' || ix == iy
              then (ix + 1, acc + points)
              else (ix, acc + points)

    charToScore :: Char -> Int
    charToScore 'X' = 10
    charToScore '/' = 10
    charToScore '-' = 0
    charToScore c = digitToInt c


camelCase :: String -> String
camelCase = histo alg . fromList
  where
    alg NilF = ""
    alg (ConsF x table) = let acc = extract table
                           in if x == '-'
                                then if null acc
                                       then ""
                                       else acc
                                else if null acc 
                                       then [toUpper x]
                                       else case nextElem table of 
                                              Just '-' -> toUpper x : acc 
                                              _ -> toUpper x : toLower (head acc) : tail acc 


coinSums :: Int -> [Int]
coinSums n = cata alg (fromList [25, 10, 5, 1]) n
  where
    alg :: ListF Int (Int -> [Int]) -> (Int -> [Int])
    alg NilF y = []
    alg (ConsF x xs) y = let (q, r) = y `divMod` x in (q : xs r)


cutVector :: [Int] -> ([Int], [Int])
cutVector xs = accu st alg (fromList xs) 0
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, s+x)
    alg NilF s = ([], [])
    alg (ConsF x (xs, ys)) s = if null xs
                                  then if null ys
                                         then if s == 0 then ([x], []) else ([], [x])
                                         else let d1 = abs (s + x - sum ys)
                                                  d2 = abs (s - x  - sum ys)
                                              in if d1 <= d2
                                                    then (x:xs, ys)
                                                    else (xs, x:ys)
                                  else (x:xs, ys)

{-
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
