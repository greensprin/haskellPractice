module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  let message = mySomeFunc "tamura yukari" 17
  print $ message
  print $ mySaiki 5

mySomeFunc :: String -> Int -> String
mySomeFunc name age = nameSan ++ ageSai
  where
    nameSan = name ++ "san"
    ageSai  = show age ++ "sai"

mySaiki :: Int -> Int
mySaiki 0 = 1
mySaiki n = n * mySaiki(n - 1)
