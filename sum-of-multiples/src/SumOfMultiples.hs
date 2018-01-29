module SumOfMultiples (sumOfMultiples) where

import Data.Set (Set,fromList)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum multiples
  where multiples = fromList(getMultiples factors limit [])

getMultiples :: [Integer] -> Integer -> [Integer] -> [Integer]
getMultiples factors limit multiples
  | length factors == 0 = multiples
  | otherwise = getMultiples xs limit [num | num <- [1..limit-1], num `mod` x == 0] ++ multiples
  where (x:xs) = factors

