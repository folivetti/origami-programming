{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import GPSB
import GPSB2
import qualified Reference.GPSB as R1
import qualified Reference.GPSB2 as R2
import Data.List ( permutations )

import Debug.Trace ( trace )

bowlingData = [("-66-9--86-5-9-36X4-",76), 
                ("36X1/1762X159/9-X4X",130), 
                ("7/6-71X9-3-6-816-7-",89), 
                ("-98116629/217-7-4452",77), 
                ("418-7/119-X7/528-1/7",102), 
                ("329/131/229/3-9-439/3",81), 
                ("9-451-9-8-9-459-413-",71), 
                ("5/-43-X8-363181X54",93), 
                ("329/22539/3--3349/44",77), 
                ("1654416/4-8/-49/4/41",86), 
                ("35428/418-26319/4-34",78), 
                ("X-77/X9--51243716-",101), 
                ("-69/8/6231443/63249-",100), 
                ("5/9/6/9-X81--0/-93/4",124), 
                ("-89/3372619/8/-6184-",90), 
                ("36--2561540/9/24319/2",85), 
                ("6-8-4-9-51428/-63143",66), 
                ("357/617-9/245/449/71",103), 
                ("54632/2411-336412371",68), 
                ("639-811633811--5-118",65), 
                ("724427X817211517/-8",89), 
                ("-834244-2/1312246-9/7",72), 
                ("X9--1519/X323/6/11",104), 
                ("X514/X263/5226275-",112), 
                ("6-627-51-61/71218/44",83), 
                ("714/-1329-529/27X34",85), 
                ("8161811632124/628/-2",76), 
                ("27279-8/X7-149-218-",96), 
                ("45-4X8/8-415112X2-",87), 
                ("2234549/5-418-636-9-",77), 
                ("-72223X219-17521/6-",78), 
                ("1852X246-6/6/X128-",104), 
                ("X5-33-452X4271X23",87), 
                ("250/5/539-X4/--8-X25",109), 
                ("72548/252-6/-6621811",74), 
                ("X9-X417/7/1/X312/4",128), 
                ("639/2/9-459-35-9456-",99), 
                ("X41228-631335176251",75), 
                ("423652116/344561X53",86), 
                ("247-5/81X818/14327/X",109), 
                ("6-9/81118143325-336/-",77), 
                ("3/-872635/5214812242",82), 
                ("8-142-15724422-67-8/1",66), 
                ("51638-165352-9-27-63",72), 
                ("X4-45X161143188/27",90), 
                ("71339/189/2-9/713271",86), 
                ("7--85/4223515/0/8/X6/",114), 
                ("447-6/-37/9/3/532/61",106), 
                ("-981339-71XXX316-",118), 
                ("456-9/7/273-62X8118",101)]
subsStrdata = [["a", "5"],
     ["!", "!"],
     ["r", "nm,xcnwqnd@#$fwkdjn3"],
     ["hi", "hihihihihihihihihihi"],
     ["############", "#"],
     ["GGGGGGGGGGGGGGGGGGGG", "G"],
     ["$$$$$$$$$$$$$$$$$$$$", "$$"],
     ["33333333333333333333", "333"],
     ["hahahahahahahahahaha", "haha"],
     ["GCTGCTGCTGCTGCTGCTGC", "GCTGC"],
     ["bbbbbbb(bb#bbbbbbbb", "bbb"],
     ["fa la la la la, la ", "la"],
     ["start and and with s", "s"],
     ["tomato", "tom"],
     ["tomatotomatotomato", "tom"],
     ["tomatotomatotomato", "to"],
     ["will be zero", "this will be zero"],
     ["APPEAR twice APPEAR", "APPEAR"],
     ["a few ending <3<3<3", "<3"],
     ["middle of this one", "of"]]

masterMindData = [["RRRR", "RRRR"],
          ["BOYG", "GYOB"],
          ["WYYW", "BBOG"],
          ["GGGB", "BGGG"],
          ["BBBB", "OOOO"],
          ["BWYG", "YWBG"],
          ["RGOW", "OGWR"],
          ["YGGB", "GYGB"],
          ["YGGB", "GYBG"],
          ["GOGY", "OGGO"],
          ["GOGR", "GOYR"],
          ["YMOO", "YMRG"],
          ["GROY", "BGOW"],
          ["GGYG", "BYBB"],
          ["WWWW", "BYWR"],
          ["RBYO", "BWBB"],
          ["RBRB", "ORBY"],
          ["WORR", "BYOW"],
          ["YOWW", "YWWR"],
          ["BRYB", "WOGG"]]

booleanData = ["t|t&f|t&t", "t|t&f|f|t|f&t&f&f|f|t|f|f|f|t&f&t&t|f", "t|t|f|f&t|f|f|t&t&t&t|t&f|f&f&t|t|f", "f|t|f|f|t&f", "f&f&t|t|t|f", "t|f|t|f&f&t&f&t&t|f", "t|t&t|t|f|f&t&t|t|f|t&t|f|t&f&t|f", "t|t&t&f&f|t&t&t&t&f", "t&t|t&f&f", "t&t&f|f&f&t|f|f&f&t", "t|t|f&t&f|f", "t&f&f|f&f|f|t|f&t|f&t|f", "t|t&f|t&t|t&f&f&t", "t&t", "t&f|f|t|t|f|f&t|t&f", "t&f&t|t", "t|t|f&f|f&t&f|t", "t|t|t&f&t&t", "f&f|f&f|f&t&f", "t&f|t&f|f&f|f&t|f|t|t|f|f|t&t&t&f&f&t", "f&f", "f&f|t|f&t|f|t&f&f&t|t&f&f&f&t|t", "t&t", "f&f|f|t&f&f&f|f|t|f|t&f|f|f&t&t|f&t", "t&f|f&t&f&t|f&t&t|f|f&f|t&t|f&f", "f&f&t&t&f&f&t|t", "t&f|t|t&t|t|f|t|t&t&f|t&t|t|t|t&t|f", "f|f|f", "t&f", "t|f&t|f|t&t", "t&f|t&f&f&f|f|f", "f|f|f|t|f|t|f|f|t&t&f|t&f&f&t&t", "t|f", "t&f|f|f&t|t&t&f|f&t&f&f|t|t|f|f&f", "t|t|f|t|t&t|t&f|f|f&t&t|f|f|t|t|t|f&t&f", "t&f|t&f|t&t&f&t|t|f|f|f&f", "t&t", "f&f&f&f&t|f", "t|t&f&t&f|t|f|f|t&f|f&f|t|t", "f&f|t&f|t", "f|t|f&f|t&t|f&f", "t|f&t&f&f|t&f&f&f|f&t&f|f|f|t", "f&t&t|t|t&t&f|f&f&t&f|f|f|f|f", "f|t|t|t|t|t&f&f|f|t&t&f|t&t&f&t|f&f", "f&t|t&t&f&f|f&f|t&t|f&t|t|f", "t&f&t|f|f|f|f|f|t&f&t|f|f&t|t|t&t&t", "t|t|t&f&f|f", "f|f|f&f|t&f&f|t&t&t|t|t|t&f", "f&t&f&t|f|f|t", "t|t|f&t|f&t&f|f|t|f&f|f&t|f&t", "t&t", "t|f&t|f|t&t|t|f|t&t|t|t|t|t&t&t|t|f|t", "f|f&t|t&t|t|t&t|t&f|t&f|t&f|f|f&f", "f|f|t&t|f|f|f&f|t|f&t", "t|t&f&f&f|f|t&f&t&f&t|f|f&f|t|t|t|f", "t|t&f|f|f|f&f|t|f|t|f|f|t|t|f", "t|t|f|t", "t&f&t|f|f&f&t&t|t&t", "t&t|f|f&t&f|f|f|t&f&t|t|t&t&f&t&f&f&t", "f&f", "t|f|f&f|f|t|f|t&f|f&f|f|f&f|f|f&f", "t&f|t", "t|t|t&t|f&t&f&t|t&t", "t&t&t|f&t&t&t&f", "f|f&t&t&t&t|f&f&t|t|f|t&f|t&f|f&f|t", "t&t|f|t|f|t&f&t|f|t|t|f|t&t&f&t|t", "t|t|f|f|f|t|t&f|f|f&t|t&f", "f&f|t&f&f|t|t&f&t|t|f&f|f", "f|f", "t&f&t&t|t&t&t&f|t&t&t", "f|f|f|f&f&t|t|f|t", "f|t|f|t&f|t|f&f&t&f|t|f&t&t|t", "t&t|f|t|f|f|t&f", "t|f&t|f&f|t|f|t|t|f&f|f&f|f", "f&t&f&t|t", "f&f|f|t&f|t&f|f|f|f|t&t", "f|f&f|f&t|f&f&t|t", "t|t|t&f|t|t|f|f|t", "t|f&f|t|f&t", "f|f&t|t&t&t|t|f&f|t&f|t&f&t&f|t|t|t|f", "t|f&f&f&t", "f&f", "t|t|f&t&f|f|t|t|f&f|f&f|t", "f&f", "t|f|f|t&t|t&t&t&f", "f&f", "f&t|t&f&f", "f&t&t|t&f|f&t|t|f|f&f|t&f", "t&f&f|t|f&f&f|t&t|t&f&f&t|t|f", "f&t&f&f&f&t|t&t&t|f&t&f&f&f|t", "f&t|t|t|t|f", "t&f|f&t&t|t|t&t|f&f|t|t|f&f&t&t|t&f", "t|t|t|f|t&f|f&t&t&t&t|f|f", "t&f|f|t&f&t&t&t|t&t|f&f&t&t&f&t|f&f", "t|t|t&f", "t|f|f|f&t&t|t&f", "t|f&t&f|f|t&f&t|f&t&t|f&f|t", "f|f&f|t&t|f|t|t", "f|t|t", "t|t"]

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

prop_bowling xs = forAll (choose (0, length bowlingData - 1)) $ \ix ->
                     let (a, b) = bowlingData !! ix
                      in bowling a == b

prop_camelcase = forAll kebabGen $ \xs -> camelCase xs == R2.camelCase xs 
  where
    kebabGen = do
      segments <- listOf1 (listOf1 (elements ['a'..'z']))
      return $ foldr1 (\a b -> a ++ "-" ++ b) segments

prop_coinSums x = coinSums coins x == R2.coinSums coins x where coins = [25, 10, 5, 1]

prop_cutVector x = let y = map getPositive x
                       (xs, ys) = cutVector y
                       (zs, ws) = R2.cutVector y
                       d1 = abs (sum xs - sum ys)
                       d2 = abs (sum zs - sum ws)
                   in d1==d2

prop_findPair x y = findPair x y == R2.findPair x y

prop_fizzBuzz x = fizzBuzz (getPositive x) == R2.fizzBuzz (getPositive x)

prop_fuelCost x = fuelCost x == R2.fuelCost x

prop_gcd x y = gcd' (getPositive x) (getPositive y) == R2.gcd' (getPositive x) (getPositive y)

prop_indicesSubStr = forAll (choose (0, length subsStrdata - 1)) $ \ix ->
                              let [a, b] = subsStrdata !! ix
                              in indicesSubStr a b == R2.indicesSubStr a b

prop_leaders xs = let ys = map getPositive xs
                  in leaders ys == R2.leaders ys

prop_luhn = forAll (vectorOf 16 (choose (0, 9))) $ \xs -> luhn xs == R2.luhn xs

prop_masterMind = forAll (choose (0, length masterMindData - 1)) $ \ix ->
                     let [a, b] = masterMindData !! ix
                     in masterMind a b == R2.masterMind a b

prop_middleChar x = middleChar x == R2.middleChar x

prop_pairedDigits = forAll (choose (0, 20)) $ \ix ->
                    forAll (vectorOf ix (choose (0, 9 :: Int))) $ \xs ->
                      let ys = concatMap show xs  :: String
                      in pairedDigits ys == R2.pairedDigits ys

prop_shoppingList xs ys = abs (shoppingList xs ys - R2.shoppingList xs ys) < 1e-6


prop_snowDay = forAll (choose (0,20)) $ \ix ->
               forAll (choose (0, 20)) $ \iy ->
               forAll (choose (0, 10)) $ \iz ->
               forAll (choose (0, 1)) $ \iw ->
                 abs (snowDay ix iy iz iw - R2.snowDay ix iy iz iw) < 1e-6

prop_solveBool = forAll (choose (0, length booleanData - 1)) $ \ix ->
                   solveBool (booleanData !! ix) == R2.solveBool (booleanData !! ix)

prop_spinWords = forAll (choose (2, 9)) $ \n ->
                 forAll (choose (2, 20)) $ \m ->
                 forAll (vectorOf m (vectorOf n (choose ('a', 'z')))) $ \xs ->
                 spinWords (unwords xs) == R2.spinWords (unwords xs)

prop_squareDigits x = squareDigits (getPositive x) == R2.squareDigits (getPositive x)

prop_subsCipher = let a = ['a' .. 'z']
                    in forAll (shuffle a) $ \b ->
                       forAll (choose (4, 100)) $ \n ->
                       forAll (vectorOf n (choose ('a', 'z'))) $ \x ->
                         subsCipher a b x == R2.subsCipher a b x

prop_twitter xs = twitter xs == R2.twitter xs

prop_vecDistance xs ys = abs (vecDistance xs ys - R2.vecDistance xs ys) < 1e-6

return []
check = $quickCheckAll

main :: IO Bool
main = check
