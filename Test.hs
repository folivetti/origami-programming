module Main where

import qualified GPSB2 as G
import qualified Reference.GPSB2 as R

main = do
    print (G.bowling "-66-9--86-5-9-36X4-")
    print (R.bowling "-66-9--86-5-9-36X4-")
