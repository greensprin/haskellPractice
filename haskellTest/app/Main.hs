{-# OPTIONS -Wall #-}

module Main where

--import Lib
--import Prac
import SHBook

main :: IO ()
main = do
  -- Prac Function
  --someFunc
  --print $ fib 6
  --print $ fib_gard 6
  --print $ fib_case_of 6
  --print $ myLength [1, 2, 3]
  --print $ mySum [1, 2, 3]
  --print $ myProduct [1, 2, 3]
  --print $ myTake 2 [1, 2, 3]
  --print $ myDrop 2 [1, 2, 3]
  --print $ myReverse [1, 2, 3]
  --print $ factWithProduct 5
  --print $ perpPoint (0, 2) (1, -1, 0)
  --print $ rot13 'A'
  --print $ isort [4, 2, 3, 1]
  --print $ show $ testDebug 5 --traceIOは使えなかった
  --print $ bsort [3, 5, 33, 496, 2, 7, 232]
  --print $ msort [3, 5, 33, 496, 2, 7, 232]
  --print $ qsort [4, 6, 9, 8, 3, 5, 1, 7, 2]
  --print $ triUnder20 [1 .. 20] [1 .. 20] [1 .. 20]
  
  --SHBook Function
  --print $ doubleMe 5
  --print $ doubleUs 4 9
  --print $ doubleSmallNumber 4
  --print $ head' [1, 2, 3]
  --print $ tell [1, 2, 3]
  --print $ firstLetter "Dracula"
  --print $ bmiTells ( calcBmis [(58.0, 1.69), (60.0, 1.80)] )
  --print $ myZip [1, 2, 3] [4, 5]
  --print $ mulTwoWithNine 2 3
  --print $ divideByTen 200
  --print $ isUpperAlphanum 'G'
  --print $ applyTwice (+2) 9
  print $ myZipWith (+) [1, 2, 3] [4, 5, 6]
