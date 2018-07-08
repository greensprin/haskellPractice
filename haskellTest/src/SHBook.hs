{-# OPTIONS -Wall -Werror #-}

module SHBook (
   doubleMe
  ,doubleUs
  ,doubleSmallNumber
  ,head'
  ,tell
  ,firstLetter
  ,calcBmis
  ,bmiTells
  ,myZip
  ,mulTwoWithNine
  ,divideByTen
  ,isUpperAlphanum
  ,applyTwice
  ,myZipWith
) where

doubleMe :: Num a => a -> a
doubleMe x = x + x

doubleUs :: Num a => a -> a -> a
doubleUs x y = x * 2 + y * 2

doubleSmallNumber :: (Num a, Ord a) => a -> a
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

head' :: [a] -> a
head' []    = error "Can't call head on and empty list, dummy!"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell []       = "empty"
tell (_:[])   = "one elem"
tell (_:_:[]) = "two elem"
tell (x:y:_)  = "so long, one elem is " ++ show x ++ " two elem is " ++ show y

firstLetter :: String -> String
firstLetter "" = "empty"
firstLetter allLtr@(x:_) = "The first letter of " ++ allLtr ++ " is " ++ [x]

bmiTells :: [Double] -> [String]
bmiTells []  = []
bmiTells (x:xs) = bmiTell x : bmiTells xs

bmiTell :: Double -> String
bmiTell x
  | x <= skinny = "skinny"
  | x <= normal = "normal"
  | x <= fat = "fat"
  | otherwise = "whale"
  where
    (skinny, normal, fat) = (18.5, 25.0, 30.0)

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ** 2]
--calcBmis xs = [bmi w h | (w, h) <- xs]
--  where
--    bmi weight height = weight / height ** 2 -- 注意）Doubleの累乗は**で、Intの累乗は^で書く

myZip :: [a] -> [b] -> [(a, b)]
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x, y) : myZip xs ys

-- カリー化
-- 2つの引数を取りそれらの掛け算と9をかけた値を返す関数
-- mulThreeであと2つ引数が必要なことは自明なので、残り2つの変数は書いていない（部分適応）
mulTwoWithNine :: Num a => a -> a -> a　-- 型宣言では必要な引数の分が分かるように宣言する
mulTwoWithNine = mulThree 9

-- 3つの値をかけた値を返す関数
mulThree :: Num a => a -> a -> a -> a
mulThree x y z = x * y * z

-- セクション
-- 中置関数でも関数の部分適応ができる
divideByTen :: Floating a => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

-- 関数を引数にとる関数
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)  -- 1引数関数が欲しい場合は部分適応を使い1引数関数にしてあげればよい

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = f x y : (myZipWith f xs ys)
