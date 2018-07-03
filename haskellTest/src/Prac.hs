module Prac
    (  fib
      ,fib_gard
      ,fib_case_of
      ,myLength
      ,mySum
      ,myProduct
      ,myTake
      ,myDrop
      ,myReverse
      ,factWithProduct
    ) where

-- Q1
-- switch case文の強化版 まずはこれで書けないかを考える
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

-- Q2
-- if文の羅列
fib_gard :: Int -> Int
fib_gard n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = fib(n - 1) + fib(n - 2)

-- Q3
-- case of 文
fib_case_of :: Int -> Int
fib_case_of n = case n of
  0 -> 0
  1 -> 1
  _ | n > 0 -> fib_case_of(n - 1) + fib_case_of(n - 2)

-- Q4
-- リスト
myLength :: [Int] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

myProduct :: [Int] -> Int
myProduct [] = 1
myProduct (x:xs) = x * myProduct xs

myTake :: Int -> [Int] -> [Int]
myTake _ []        = []
myTake n _ | n < 1 = []
myTake n (x:xs)    = x : myTake (n-1) xs

myDrop :: Int -> [Int] -> [Int]
myDrop _ []         = []
myDrop n xs | n < 1 = xs
myDrop n (x:xs)     = myDrop (n-1) xs

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Q5
factWithProduct :: Int -> Int
factWithProduct n | n > 0 = myProduct [1..n]

-- 次回タプルから
