{-# OPTIONS_GHC -Wall #-}
module HW01 where

creditCardNum :: Integer
creditCardNum = 5594589764218858

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Convert positive Integers to a list of digits.
toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = zipWith
  (\x y -> if x `mod` 2 == 0 then y * 2 else y) [(1::Integer)..]

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map f where
  f m
    | m <= 9 = m
    | otherwise = sumDigits $ toRevDigits m

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn = (== 0) . (`mod` 10) . sumDigits . doubleEveryOther . toRevDigits

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a c b
  | n <= 0    = []
  | otherwise = hanoi (n - 1) a b c ++
      ((a, c) : hanoi (n - 1) b c a)

----------------------------------------------

toDigits :: Integer -> [Integer]
toDigits n = reverse $ toRevDigits n

doubleEveryOtherInt :: Integer -> [Integer]
doubleEveryOtherInt n = doubleEveryOther $ toDigits n

doubleEveryOtherIntRev :: Integer -> [Integer]
doubleEveryOtherIntRev n = doubleEveryOther $ toRevDigits n