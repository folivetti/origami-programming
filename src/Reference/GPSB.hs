-----------------------------------------------------------------------------
-- |
-- Module      :  Reference.GPSB 
-- Copyright   :  (c) Fabricio Olivetti 
-- License     :  BSD3
-- Maintainer  :  fabricio.olivetti@gmail.com
-- Stability   :  experimental
-- Portability :  TupleSections
--
-- Reference solution for the GPSB - general program synthesis benchmark problems 
--
-----------------------------------------------------------------------------

{-# LANGUAGE TupleSections #-}
module Reference.GPSB where

import Data.Char ( isSpace )
import Data.List ( find, sort, group, intercalate )
import Data.Maybe ( fromMaybe )
import qualified Data.Map as M

-- The solutions that do not require recursion and 
-- does not have a counterpart in module GPSB are marked with *

-- Number IO *
numberIO :: Int -> Double -> Double
numberIO x y = fromIntegral x + y

-- Small Or Large *
smallOrLarge :: Int -> String
smallOrLarge x | x < 1000 = "small"
               | x >= 2000 = "large"
               | otherwise = ""

-- for loop index
forLoopIndex :: Int -> Int -> Int -> [Int]
forLoopIndex start end step = [start, start+step .. end-1]

-- compare string lengths *
cmpStrLen :: String -> String -> String -> Bool
cmpStrLen n1 n2 n3 = length n1 < length n2 && length n2 < length n3

-- double letters
doubleLetters :: String -> String
doubleLetters = concatMap (\c -> if isLetter c then [c,c] else if c == '!' then "!!!" else [c])
  where isLetter c = c `elem` (['A' .. 'Z'] <> ['a' .. 'z'])

-- collatz
collatz :: Int -> Int
collatz x = length $ go x
  where
     go 1 = [1]
     go x | even x = x : go (div x 2)
          | otherwise = x : go (div (3*x + 1) 2)

-- replace space with newline
replaceSpaceCount :: String -> (String, Int)
replaceSpaceCount xs = (map (\c -> if c==' ' then '\n' else c) xs, length (filter (/=' ') xs))

-- string difference
stringDiffs :: String -> String -> [(Int, (Char, Char))]
stringDiffs xs ys = filter (\(ix, (x, y)) -> x/=y) $ zip [0..] $ zip xs ys

-- Even Squares
evenSquares :: Int -> [Int]
evenSquares n = filter (<n) $ filter even $ map (^2) [n, n-1 .. 1]

-- Wallis Pi
wallisPi :: Int -> Double
wallisPi n = (0.5 *) $ product $ map (\x -> fromIntegral (4 * x ^ 2) / fromIntegral ((2*x - 1)*(2*x + 1))) [1 .. n]

-- String Length Backwards
strLenBack :: [String] -> [Int]
strLenBack xs = map length $ reverse xs

-- Last Index of Zero
lastIndexZero :: [Int] -> Int
lastIndexZero xs = fromMaybe (-1) $ fmap fst $ find ((==0).snd) $ reverse $ zip [0..] xs

-- Vector average
vecAvg :: [Double] -> Double
vecAvg xs = sum xs / (fromIntegral (length xs))

-- Count Odds
countOdds :: [Int] -> Int
countOdds xs = length (filter odd xs)

-- Mirror Image
mirrorImage :: [Int] -> [Int] -> Bool
mirrorImage xs ys = xs == reverse ys

-- Super Anagram
superAnagram :: String -> String -> Bool
superAnagram xs ys = and [M.member x ys' && c <= ys' M.! x | (x,c) <- xs']
  where
    -- groupCnt will generate a map of letter -> count for a string
    groupCnt = map (\zs -> (head zs, length zs)) . group . sort
    xs' = groupCnt xs
    ys' = M.fromList $ groupCnt ys

-- Sum of Squares
sumOfSquares :: Int -> Int
sumOfSquares x = sum $ map (^2) [1 .. x]

-- Vectors summed
sumOfVecs :: [Int] -> ([Int] -> [Int])
sumOfVecs xs ys = zipWith (+) xs ys

-- X-Word Lines
xWordLines :: Int -> String -> String
xWordLines n xs = go n xs
  where
    go _ "" = "" -- no more words to split
    go 0 (' ':ys) = '\n' : go n ys  -- we've reached n words, print a \n
    go i ('\n':ys) | i > 0 = ' ' : go i ys -- a newline but it should be word
    go i (' ':ys) = ' ' : go (i-1) ys -- a word but before reaching n words
    go i (y : ys) = y : go i ys -- a letter

-- Pig Latin
pigLatin :: [Char] -> [Char]
pigLatin = go
  where
   go "" = ""
   go " " = " "
   go xs = let x = takeWhile (/=' ') xs
               y = dropWhile (/=' ') xs
            in f x <> (if null y then [] else ' ' : go (tail y))
   f "" = ""
   f x = if head x `elem` "aeiouAEIOU" then x <> "ay" else tail x <> (head x : "ay")

-- Negative to Zero
negativeToZero :: [Int] -> [Int]
negativeToZero xs = map (\x -> if x < 0 then 0 else x) xs

-- Scrabble score
scrabbleScore :: String -> Int
scrabbleScore xs = sum $ map getScore xs
  where 
    getScore x = M.findWithDefault 0 x scores
    scores = M.fromList $ concat [map (,1) "AEIOULNSTR", map (,2) "DG", map (,4) "FHVWY", [('K',5)], map (,8) "JX", map (,10) "QX"]

-- Word Stats: this problem was broken down into three subproblems
wordStats :: String -> (M.Map Int Int, Int, Double)
wordStats xs = (wordDist xs, lineCount xs, avgLineLen xs)

wordDist :: String -> M.Map Int Int
wordDist xs = M.fromListWith (+) $ map (\x -> (length x, 1)) $ words xs

lineCount :: String -> Int
lineCount xs = length $ lines $ dropWhile (=='\n') xs

avgLineLen :: String -> Double
avgLineLen "" = 0
avgLineLen xs | null (strip xs) = 0
              | otherwise = avg $ filter (/=0) $ map (fromIntegral . length) $ lines $ strip xs
  where avg ys = sum ys / (fromIntegral (length ys))
        strip ys = reverse $ dropWhile (`elem` "\n\r") $ reverse $ dropWhile (`elem` "\n\r") ys

-- Checksum
checksum :: String -> Char
checksum xs = toEnum $ (fromEnum ' ' +) $ (`mod` 64) $ sum $ map fromEnum xs

-- Digits
digits :: Int -> [Int]
digits 0 = []
digits x | abs x < 10 = x `rem` 10 : digits (x `quot` 10)
         | otherwise = abs (x `rem` 10) : digits (x `quot` 10) 

-- Grade
grade :: [(Double, Char)] -> (Double -> Char)
grade thrs n = go thrs
  where
    go [] = 'F'
    go ((t1,t2):ts) = if n >= t1 then t2 else go ts

-- Median *
median x y z = head $ tail $ sort [x,y,z]
             
-- Smallest *
smallest x y z w = minimum [x,y,z,w]

-- Syllables
syllables :: String -> Int
syllables xs = length $ filter (`elem` "AEIOUYaeiouy") xs