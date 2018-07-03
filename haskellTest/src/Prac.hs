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
      ,perpPoint
      ,rot13
    ) where

-- Q7で使用
import Data.Char

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

-- Q6 taple
-- 割り算を使用する際は小数点が出る可能性があるため、doubleで型を定義する
perpPoint :: (Double, Double) -> (Double, Double, Double) -> (Double, Double)
perpPoint (p, q) (a, b, c) = (x, y)
  where
    x = (a*c + b*d) / (a*a + b*b) :: Double
    y = (b*c - a*d) / (a*a + b*b) :: Double
    d = b*p - a*q :: Double

-- Q7 ROT13 大文字のみ対応 && || などで条件分岐すれば小文字にも対応可
rot13 :: Char -> Char
rot13 chr1
  | (ord chr1) < (ord 'N') = chr $ (ord chr1) + 13
  | otherwise              = chr $ (ord chr1) - 13

-- Q8 

