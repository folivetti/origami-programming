-----------------------------------------------------------------------------
-- |
-- Module      :  Reference.GPSB 
-- Copyright   :  (c) Fabricio Olivetti 
-- License     :  BSD3
-- Maintainer  :  fabricio.olivetti@gmail.com
-- Stability   :  experimental
-- Portability :  TupleSections, MultiWayIf
--
-- Origami solution for the GPSB - general program synthesis benchmark problems 
--
-- for catamorphisms the template is (the algorithm must evolve the holes "_")
-- f = cata alg . fromList
-- alg NilF = _
-- alg (ConsF x xs) = _
--
-- for anamorphisms the template is (the algorithm must evolve the holes "_")
--
-- f = toList . ana coalg
-- coalg seed | _ -> NilF
--            | otherwise -> ConsF _ _
--
-- for accumorphisms the template is (the algorithm must evolve the holes "_")
--
-- f x = accu st alg (fromList x) _
-- st NilF s = NilF
-- st (ConsF x xs) s = ConsF x (xs, _)
-- alg NilF s = _
-- alg (ConsF x xs) s = _
-- 
-- for hylomorphism the template is (the algorithm must evolve the holes "_")
--
-- f = hylo alg coalg
-- alg and coalg follow the same template as cata and ana
--
-- The solutions are sorted by recursion scheme (cata, accu, ana, hylo) and
-- by the order they appear in the paper:
--
-- Helmuth, Thomas, and Lee Spector. "General program synthesis benchmark suite." 
-- Proceedings of the 2015 Annual Conference on Genetic and Evolutionary Computation. 2015.
-----------------------------------------------------------------------------

{-# language TupleSections #-}
{-# language MultiWayIf #-}
module GPSB where

import Rec

import Data.Semigroup
import Data.List
import Debug.Trace ( trace )
import qualified Data.Map.Strict as M
import Data.Bifunctor (bimap, first, second)

-- Catamorphisms

-- double letters
doubleLetters :: String -> String
doubleLetters = cata alg . fromList
  where
    alg NilF         = []
    alg (ConsF x xs) = if | x == '!'   -> "!!!" <> xs
                          | isLetter x -> [x,x] <> xs 
                          | otherwise  -> x:xs  

-- replace space with newline
replaceSpaceCount :: String -> (String, Int)
replaceSpaceCount = cata alg . fromList
  where
    alg NilF = ("", 0) 
    alg (ConsF x xs) = if | x == ' '  -> ('\n' : fst xs, snd xs) 
                          | otherwise -> (x:fst xs, 1 + snd xs)

-- string length backwards
strLenBack :: [String] -> [Int]
strLenBack = cata alg . fromList
  where
    alg NilF = []
    alg (ConsF x xs) = xs <> [length x]

-- Count odds
countOdds :: [Int] -> Int
countOdds = cata alg . fromList
  where
    alg NilF = 0
    alg (ConsF x xs) = xs + mod x 2

-- Mirror image
mirrorImage :: [Int] -> ([Int] -> Bool)
mirrorImage = cata alg . fromList
  where
    alg NilF = \ys -> null ys -- ys must be null at the end of xs
    alg (ConsF x xs) = \ys -> (not.null) ys && (x == last ys) && xs (init ys) -- ys cannot be null AND the current char of x must be the last char of ys AND the function applied to the init of ys must be true

-- super anagram
superAnagram :: String -> (String -> Bool)
superAnagram = cata alg . fromList
  where
    alg NilF = \ys -> True -- ys is a super anagram of an empty list
    alg (ConsF x xs) = \ys ->  x `elem` ys && xs (delete x ys) -- ys must still have a char, and it must contain x, and it must be true for ys after removing x

-- vectors summed
sumOfVecs :: [Int] -> ([Int] -> [Int])
sumOfVecs xs ys = cata alg (fromList xs) ys
  where
    alg NilF = \zs -> []
    alg (ConsF x xs) = \zs -> if | null zs   -> []
                                 | otherwise -> (x + head zs) : xs (tail zs)

-- negative to zero
negativeToZero :: [Int] -> [Int]
negativeToZero = cata alg . fromList
  where
    alg NilF = []
    alg (ConsF x xs) = max 0 x : xs

-- scrabble score
-- should we let GP figure it out the map?
scrabbleScore :: String -> Int
scrabbleScore = cata alg . fromList
  where
   alg NilF = 0
   alg (ConsF x xs) = getScore x + xs

   getScore x = M.findWithDefault 0 x scores
   scores = M.fromList $ concat [map (,1) "AEIOULNSTR", map (,2) "DG", map (,4) "FHVWY", [('K',5)], map (,8) "JX", map (,10) "QX"]

-- grade
grade :: [(Double, Char)] -> (Double -> Char)
grade thrs n = cata alg (fromList thrs) n
  where
    alg NilF n = 'F'
    alg (ConsF x xs) n = if | n >= fst x -> snd x 
                            | otherwise  -> xs n

-- syllables
syllables :: String -> Int
syllables = cata alg . fromList
  where
    alg NilF = 0
    alg (ConsF x xs) = if | x `elem` "aeiouyAEIOUY" -> xs + 1
                          | otherwise               -> xs


-- * Accumorphisms

-- string differences
stringDiffs :: String -> (String -> [(Int, (Char, Char))])
stringDiffs xs ys = accu st alg (fromList xs) 0 ys
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, s+1)
    alg NilF s = \zs -> []
    alg (ConsF x xs) s = \zs -> if | null zs -> []
                                   | x /= head zs ->  (s, (x, head zs)) : xs (tail zs)
                                   | otherwise -> xs (tail zs)

-- string differences using cata AND indexed list
stringDiffs' :: String -> String -> [(Int, (Char, Char))]
stringDiffs' xs ys = cata alg (fromIList xs) ys
  where
    alg INilF = \zs -> []
    alg (IConsF ix x xs) = \zs -> if | null zs -> []
                                     | x /= head zs -> (ix, (x, head zs)) : xs (tail zs)
                                     | otherwise -> xs (tail zs)
-- Last index of zero
-- note: this is much nicer with Maybe
lastIndexZero :: [Int] -> Int
lastIndexZero xs = accu st alg (fromList xs) 0
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, s+1)

    alg NilF s = -1
    alg (ConsF x xs) s = if x == 0 && xs == -1 then s else xs

-- Last index of zero with cata and indexed list
lastIndexZero' :: [Int] -> Int
lastIndexZero' xs = cata alg (fromIList xs)
  where
    alg INilF = -1
    alg (IConsF ix x xs) = if x == 0 && xs == -1 then ix else xs

-- Vector Average
vecAvg :: [Double] -> Double
vecAvg xs = accu st alg (fromList xs) (0.0, 0.0)
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, (fst s + x, snd s + 1))
    alg NilF s = uncurry (/) s
    alg (ConsF x xs) s = xs

-- vector average with cata allowing a post-process function
-- of type Double -> Double
vecAvg' :: [Double] -> Double
vecAvg' xs = (/ length' xs) $ cata alg (fromList xs)
  where
    alg NilF = 0
    alg (ConsF x xs) = x + xs

-- X-word lines
xWordLines :: Int -> String -> String
xWordLines n xs = accu st alg (fromList xs) 1
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, if x /= ' ' && x /= '\n'
                                       then s
                                       else s + 1)
    alg NilF s = []
    alg (ConsF x xs) s = if | x == ' ' && mod s n == 0 -> '\n' : xs
                            | x == '\n' && mod s n /= 0 -> ' ' : xs
                            | otherwise -> x : xs

-- x-words lines allowing pre-processing of type String -> [String]
-- (f a -> f (f a))
-- and using indexed list
xWordLines' :: Int -> String -> String
xWordLines' n xs = let xs' = words xs
                    in cata alg (fromIList xs')
  where
    alg INilF = ""
    alg (IConsF ix x xs) 
       | mod (ix+1) n == (n-1) = x <> ('\n' : xs)
       | otherwise = x <> (' ' : xs)

-- pig latin
pigLatin :: [Char] -> [Char]
pigLatin cs = accu st alg (fromList cs) ("", "")
  where
    st NilF s = NilF
    st (ConsF x xs) (s1, s2) = ConsF x (xs, (
            if | x == ' ' -> snoc x (s1 <> s2)
               | null s2 && not (elem x "AEIOUaeiou") -> s1
               | otherwise -> snoc x s1
          , if | x == ' ' -> ""
               | null s2 && elem x "AEIOUaeiou" -> "ay"
               | null s2 && not (elem x "AEIOUaeiou") -> x:"ay"
               | otherwise -> s2
            ))
    alg NilF (s1, s2) = s1 <> s2
    alg (ConsF x xs) s = xs

-- pig latin using cata allowing pre-process function
-- of type String -> [String] (f a -> f (f a))
pigLatin' :: String -> String
pigLatin' xs = let xs' = words xs
                in cata alg (fromList xs')
  where
    alg NilF = ""
    alg (ConsF x xs)
       | head x `elem` "AEIOUaeiou" = x <> "ay" <> xs
       | otherwise = tail x <> (head x : "ay") <> xs

-- Word stats, split into three for clarity
wordStats :: String -> (M.Map Int Int, Int, Double)
wordStats xs = (wordDist xs, lineCount xs, avgLineLen xs)

wordDist :: String -> M.Map Int Int
wordDist xs = accu st alg (fromList xs) ""
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, if elem x " \n\t\f\v\r" then "" else (x:s))
    alg NilF s = if null s then M.empty else M.singleton (length s) 1
    alg (ConsF x xs) s = if elem x " \n\t\f\v\r" && (not.null) s then M.insertWith (+) (length s) 1 xs else xs

lineCount :: String -> Int
lineCount xs = accu st alg (fromList xs) ""
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, if x == '\n' then "" else (x:s))
    alg NilF s = if null s then 0 else 1
    alg (ConsF x xs) s = if x == '\n' && (not.null) s then xs+1 else xs

avgLineLen :: String -> Double
avgLineLen xs = accu st alg (fromList xs) (0.0, 0.0, 0.0)
  where
    st NilF s = NilF
    st (ConsF x xs) (lineLen, totLines, sumLen) = ConsF x (xs, if x /= '\n' then (lineLen + 1, totLines, sumLen) else (0.0, totLines + min lineLen 1, sumLen + lineLen))

    alg NilF (lineLen, totLines, sumLen) = if lineLen == 0 then (if totLines == 0 then 0 else sumLen / totLines) else (sumLen + lineLen) / (totLines + 1)
    alg (ConsF x xs) (lineLen, totLines, sumLen) = xs

-- Word stats, split into three for clarity allowing a single pre-processing function
-- of type String -> [String] (f a -> f (f a)) and post-processing of type b -> b
wordDist' :: String -> M.Map Int Int
wordDist' xs = let xs' = words xs
                in cata alg (fromList xs')
  where
   alg NilF = M.empty
   alg (ConsF x xs) = M.insertWith (+) (length x) 1 xs

lineCount' :: String -> Int
lineCount' xs = let xs' = lines xs
                 in cata alg (fromList xs')
  where
    alg NilF = 0
    alg (ConsF x xs) = 1 + xs

avgLineLen' :: String -> Double
avgLineLen' xs = let xs' = lines xs
                  in (/ length' xs') $ cata alg (fromList xs')
  where
    alg NilF = 0
    alg (ConsF x xs) = length' x + xs

-- Checksum
checksum :: String -> Char
checksum xs = accu st alg (fromList xs) 0
  where
    st NilF s = NilF
    st (ConsF x xs) s = ConsF x (xs, toEnum (mod (fromEnum x + fromEnum s) 64))

    alg NilF s = toEnum (fromEnum s + 32) -- 32 = fromEnum ' '
    alg (ConsF x xs) s = xs

-- checksum using cata with a post-processing function
-- of type Char -> Char (b -> b)
checksum' :: String -> Char
checksum' xs = addChars ' ' $ cata alg (fromList xs)
  where
   alg NilF = '\0'
   alg (ConsF x xs) = modChar (addChars x xs) 64

   addChars :: Char -> Char -> Char
   addChars c1 c2 = toEnum (fromEnum c1 + fromEnum c2)
   modChar :: Char -> Int -> Char
   modChar c n = toEnum (fromEnum c `mod` n)

-- * Anamorphisms

-- For Loop Index
forLoopIndex :: Int -> Int -> Int -> [Int]
forLoopIndex start end step = toList $ ana coalg start
  where
    coalg seed
      | seed >= end = NilF
      | otherwise   = ConsF seed (seed + step)


-- Digits
digits :: Int -> [Int]
digits = toList . ana coalg
  where
      coalg seed
        | seed == 0 = NilF
        | abs seed < 10 = ConsF (rem seed 10) (quot seed 10)
        | otherwise = ConsF (abs (rem seed 10)) (quot seed 10)

-- * Hylo

-- Collatz
collatz :: Int -> Int
collatz = hylo alg coalg
  where
      alg NilF = 1
      alg (ConsF x xs) = 1 + xs

      coalg seed
        | seed == 1 = NilF
        | even seed = ConsF seed (div seed 2)
        | otherwise = ConsF seed (div (3*seed + 1) 2)

-- Even Squares
evenSquares :: Int -> [Int]
evenSquares n = hylo alg coalg n
  where
    alg NilF = []
    alg (ConsF x xs) = if x >= n then xs else x : xs
    coalg seed
      | seed^2 <= 1 = NilF
      | otherwise   = ConsF (seed ^ 2) (seed - 1 - (1 - mod seed 2))

-- Even squares using ana if we allow the argument
-- within scope of coalg and not using arg as the initial seed
-- reverse is not actually needed, just to make testing easier.
evenSquares' :: Int -> [Int]
evenSquares' n = reverse . toList $ ana coalg 2
  where
    coalg seed
      | seed^2 >= n = NilF
      | otherwise = ConsF (seed^2) (seed + 2)

-- Wallis Pi
wallisPi :: Int -> Double
wallisPi = hylo alg coalg
  where
    alg NilF = 0.5
    alg (ConsF x xs) = x * xs
    coalg x = if x == 0
                then NilF
                else ConsF (fromIntegral (4 * x ^ 2) / fromIntegral ((2*x - 1)*(2*x + 1))) (x-1)

-- Sum of squares
sumOfSquares :: Int -> Int
sumOfSquares = hylo alg coalg
  where
    coalg x = if x == 0 then NilF else ConsF x (x-1)

    alg NilF = 0
    alg (ConsF x xs) = x^2 + xs


-- * Utility functions

-- snoc insert one element at the end of the list
snoc x xs = xs <> [x]

-- returns True if a char is one of the leters of the alphabet
isLetter c = c `elem` (['A' .. 'Z'] <> ['a' .. 'z'])

length' = fromIntegral . length