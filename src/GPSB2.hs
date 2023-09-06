{-# language TupleSections #-}
{-# language MultiWayIf #-}
module GPSB2 where

import Data.Char ( digitToInt, toUpper, toLower )
import Data.List ( tails, findIndex )
import qualified Data.Map.Strict as M
import Rec
import Debug.Trace ( trace )
import GPSB (xWordLines)

basement :: [Int] -> Maybe Int
basement xs = accu st alg (fromIList xs) 0
  where
    st INilF s = INilF
    st (IConsF ix x xs) s = IConsF ix x (xs, s+x)
    alg INilF s = Nothing
    alg (IConsF ix x xs) s = if x + s < 0 then Just ix else xs

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


diceGame :: Int -> Int -> Double
diceGame n m = 1.0 - (fromIntegral m + 1) / (2 * fromIntegral n)


findPair :: [Int] -> Int -> (Int, Int)
findPair xs n = histo alg (fromList xs)
  where
    alg NilF = (0, 0)
    alg (ConsF x table) = case findElem (n - x) table of
                               Nothing -> extract table
                               Just y -> (x, y)
    findElem x (acc :< NilF) = Nothing
    findElem x (acc :< ConsF y tbl) = if x == y then Just x else findElem x tbl

fizzBuzz :: Int -> [String]
fizzBuzz n = toList (ana coalg 1)
  where
    coalg x
         | x > n = NilF
         | x `mod` 3 == 0 && x `mod` 5 == 0 = ConsF "FizzBuzz" (x+1)
         | x `mod` 3 == 0 = ConsF "Fizz" (x+1)
         | x `mod` 5 == 0 = ConsF "Buzz" (x+1)
         | otherwise = ConsF (show x) (x+1)

fuelCost :: [Int] -> Int
fuelCost = cata alg . fromList
  where
    alg NilF = 0
    alg (ConsF x xs) = (x `div` 3 - 2) + xs

gcd' :: Int -> Int -> Int
gcd' x y = hylo alg coalg (x, y)
  where
    alg (Value x) = x
    alg (Delayed x) = x

    coalg (a, b) = case a `mod` b of
                     0 -> Value b
                     m -> Delayed (b, m)

indicesSubStr :: String -> String -> [Int]
indicesSubStr xs target = histo alg (fromIList xs)
  where
    alg INilF = []
    alg (IConsF ix x table) = if target == (x : takeTbl (length target - 1) table)
                                 then ix : extract table
                                 else extract table
    takeTbl 0 _ = []
    takeTbl n (_ :< INilF) = []
    takeTbl n (_ :< IConsF _ x tbl) = x : takeTbl (n-1) tbl

leaders :: [Int] -> [Int]
leaders = histo alg . fromList
  where
    alg NilF = []
    alg (ConsF x table) = if all (<=x) (tbl2list table)
                             then x : extract table
                             else extract table

    tbl2list (_ :< NilF) = []
    tbl2list (_ :< ConsF x tbl) = x : tbl2list tbl


luhn :: [Int] -> Int
luhn = cata alg . fromIList
  where
    alg INilF = 0
    alg (IConsF ix x xs) = if ix `mod` 2 == 0
                              then x + xs
                              else let y = 2*x
                                   in if y >= 10
                                         then y - 9 + xs
                                         else y + xs


masterMind :: String -> String -> (Int, Int)
masterMind code guess = let (f1,f2) = mutu alg1 alg2  in (f1 (fromList guess) code, f2 (fromList guess) code)
  where
    alg1 NilF ys = 0
    alg1 (ConsF x xs) ys = if (not.null) ys && x == (head ys)
                              then 1 + (fst xs) (tail ys)
                              else (fst xs (tail ys))
    alg2 NilF ys = 0
    alg2 (ConsF x xs) ys = if x `elem` ys then 1 + (snd xs ys) else (snd xs ys)


middleChar :: String -> String
middleChar xs = histo alg (fromIList xs)
  where
    alg INilF = []
    alg (IConsF ix x table) = let sz = length (tbl2list table)
                              in if sz == ix
                                 then [x]
                                 else if sz > ix && null (extract table)
                                        then [x, nextElem' table]
                                        else extract table
    tbl2list (_ :< INilF) = []
    tbl2list (_ :< IConsF ix x tbl) = x : tbl2list tbl
    nextElem' (_ :< INilF) = undefined
    nextElem' (_ :< IConsF ix x tbl) = x

pairedDigits :: String -> Int
pairedDigits = histo alg . fromList
  where
    alg NilF = 0
    alg (ConsF x table) = case nextElem table of
                               Nothing -> extract table
                               Just y  -> if digitToInt x == digitToInt y then digitToInt x + extract table else extract table

shoppingList :: [Double] -> [Double] -> Double
shoppingList xs ys = cata alg (fromList xs) ys
  where
    alg NilF zs = 0
    alg (ConsF x f) zs = if null zs
                             then f zs
                             else x - x * head zs / 100 + f (tail zs)


snowDay :: Int -> Double -> Double -> Double -> Double
snowDay hours snow rate melt = snow*(melt ^ hours) + rate * melt * (melt^hours - 1) / (melt - 1)


solveBool :: String -> Bool
solveBool = fromRight . cata alg . fromList . reverse
  where
    fromRight (Right x) = x

    alg NilF = Right False
    alg (ConsF x xs) = case x of
                          't' -> case xs of
                                    Right _ -> Right True
                                    Left f  -> Right (f True)
                          'f' -> case xs of
                                      Right _ -> Right False
                                      Left f -> Right (f False)
                          '|' -> case xs of
                                      Right v -> Left (v ||)
                                      Left  f -> Left f
                          '&' -> case xs of
                                      Right v -> Left (v &&)
                                      Left  f -> Left f

-- can do it without unwords . words, but meh
spinWords :: String -> String
spinWords xs = accu st alg (fromList xs) ""
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, if (not.null) s && head s == ' ' then [x] else (x:s))
    alg NilF s = if length s >= 5 then s else reverse s
    alg (ConsF x xs) s = if (not.null) s && head s == ' '
                          then
                             if length s >= 6
                                then tail s <> (' ' : xs)
                                else reverse (tail s) <> (' ' : xs)
                          else xs
    {-
  unwords . cata alg . fromList . words
  where
    alg NilF = []
    alg (ConsF x xs) = if length x >= 5
                          then reverse x : xs
                          else x : xs
                          -}
squareDigits :: Int -> String
squareDigits = hylo alg coalg
  where
    alg NilF = "0"
    alg (ConsF x xs) = if xs == "0" then show x else xs <> show x

    coalg x = case x of
                0 -> NilF
                y -> ConsF ((y `mod` 10) ^ 2) (y `div` 10)

subsCipher :: String -> String -> String -> String
subsCipher xs ys code = cata alg (fromList xs) ys code
  where
    alg NilF ys zs = map toLower zs
    alg (ConsF x xs) ys zs = if null ys
                               then zs
                               else xs (tail ys) (replace x (head ys) zs)
    replace x y xs = map (\z -> if z == x then toUpper y else z) xs

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
