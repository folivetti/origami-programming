{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import GPSB
import GPSB2
import qualified Reference.GPSB as R1
import qualified Reference.GPSB2 as R2

prop_forLoopIndex :: Property
prop_forLoopIndex = forAll (choose (-1000, 1000)) $ \start ->
                    forAll (choose (1, 10)) $ \step ->
                    forAll (choose (start+step, 1000)) $ \end ->
                       forLoopIndex' start end step
  where forLoopIndex' start end step = forLoopIndex start end step == R1.forLoopIndex start end step

prop_doubleLetters xs = doubleLetters xs == R1.doubleLetters xs

prop_collatz x = collatz (getPositive x) == R1.collatz (getPositive x)

prop_replaceSpaceCount xs = replaceSpaceCount xs == R1.replaceSpaceCount xs

prop_stringDiffs xs ys = stringDiffs xs ys == R1.stringDiffs xs ys

prop_evenSquares x = evenSquares' (getPositive x) == R1.evenSquares (getPositive x)

prop_wallisPi x = wallisPi (getPositive x) == R1.wallisPi (getPositive x)

prop_strLenBack x = strLenBack x == R1.strLenBack x

prop_lastIndexZero x = lastIndexZero x == R1.lastIndexZero x

prop_vecAvg x = vecAvg (getNonEmpty x) == R1.vecAvg (getNonEmpty x)

prop_countOdds x = countOdds x == R1.countOdds x

prop_mirrorImage x y = mirrorImage x y == R1.mirrorImage x y

prop_superAnagram x y = superAnagram x y == R1.superAnagram x y

prop_sumOfSquares x = sumOfSquares (getPositive x) == R1.sumOfSquares (getPositive x)

prop_sumOfVecs x y = sumOfVecs x y == R1.sumOfVecs x y

prop_xWordLines x y = xWordLines (getPositive x) y == R1.xWordLines (getPositive x) y

prop_pigLatin x = pigLatin x == R1.pigLatin x

prop_negativeToZero x = negativeToZero x == R1.negativeToZero x

prop_scrabbleScore x = scrabbleScore x == R1.scrabbleScore x

prop_wordDist x = wordDist (getNonEmpty x) == R1.wordDist (getNonEmpty x)

prop_lineCount x = lineCount (getNonEmpty x) == R1.lineCount (getNonEmpty x)

prop_avgLineLen x = avgLineLen (getNonEmpty x) == R1.avgLineLen (getNonEmpty x)

prop_checksum x = checksum x == R1.checksum x

prop_digits x = digits x == R1.digits x

prop_grade = forAll (choose (6, 10)) $ \a ->
             forAll (choose (5, a-1)) $ \b ->
             forAll (choose (4, b-1)) $ \c ->
             forAll (choose (3, c-1)) $ \d ->
             forAll (choose (0, 10)) $ \n ->
               grade [(a, 'A'), (b, 'B'), (c, 'C'), (d, 'D')] n == R1.grade [(a, 'A'), (b, 'B'), (c, 'C'), (d, 'D')] n

prop_syllables x = syllables x == R1.syllables x

-- GPSB 2
prop_basement x = basement x == R2.basement x

prop_vecDistance xs ys = abs (vecDistance xs ys - R2.vecDistance xs ys) < 1e-6

prop_twitter xs = twitter xs == R2.twitter xs

return []
check = $quickCheckAll

main :: IO Bool
main = check