{-# OPTIONS -Wall -Werror #-}

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
      ,isort
      ,testDebug
      ,bsort
      ,msort
      ,qsort
      ,triUnder20
    ) where

-- Q7で使用
import Data.Char
-- debugで使用
import Debug.Trace

-- 第1章
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
  _
    | n > 0 -> fib_case_of(n - 1) + fib_case_of(n - 2)
    | otherwise -> 0

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
myDrop n xs         = myDrop (n-1) xs

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- Q5 
factWithProduct :: Int -> Int
factWithProduct n
  | n > 0 = myProduct [1..n]
  | otherwise = 0
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

-- example sort
insert :: Int -> [Int] -> [Int]
insert x [] = [x]
insert x (y:ys)
  | x < y     = x:y:ys
  | otherwise = y : insert x ys

isort :: [Int] -> [Int]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- how to debug test 
--testDebug Int -> Int
testDebug :: Show a => a -> a
testDebug x = trace( "test" ++ show x ) x

-- Q8 buble sort
-- bswap
-- 最小値を先頭にし、以降はそれ以外となったリストを返す
bswap :: [Int] -> [Int]
bswap []  = []
bswap [x] = [x]
bswap (x:xs)
  | x > y     = y:x:ys
  | otherwise = x:y:ys
    where
      (y:ys) = bswap xs

-- | buble sort
--
--   先頭を取り除き、それ以外のリストの数値から最小の値を先頭にするよう並び替える
bsort :: [Int] -> [Int]
bsort [] = []
bsort xs = y : bsort ys
  where (y:ys) = bswap xs

-- Q9 merge sort
-- | merge sort
--   
--   行きはリストを分割し、帰りに2つのリストを比較し、小さいものを順に返す
msort :: [Int] -> [Int]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
  where
    ys   = take hlen xs
    zs   = drop hlen xs
    hlen = (length xs) `div` 2

-- 2つの配列の先頭を比べ、小さいものを取り出す、そのほかものはさらにこの関数に入れ比較
-- 最終的に2つのリスト内のものが小さい順にならんで結合されたものが出てくる
merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) 
  | x < y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys

-- Q10
-- | quick sort
--   
--   先頭の数値以下のものと、以上のものでリスト内の要素を分け、
--   先頭の数値を真ん中とし結合
--
--   分割されたリストはそれぞれさらにソートを行う

qsort :: [Int] -> [Int]
qsort [] = []
qsort (n:xs) = qsort lt ++ [n] ++ qsort gteq
  where
    lt   = [x | x <- xs, x <  n]
    gteq = [x | x <- xs, x >= n]

-- Q11
triUnder20 :: (Num a, Eq a) => [a] -> [a] -> [a] -> [(a, a, a)]
triUnder20 xs ys zs = [(x, y, z) | x <- xs, y <- ys, z <- zs, z*z == x*x + y*y]

-- 第2章

