module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | n == 1 = Just Deficient
  | aliquotsSum > n = Just Abundant
  | aliquotsSum == n = Just Perfect
  | aliquotsSum < n = Just Deficient
  where aliquotsSum = (sum $ aliquots n) - n

aliquots :: Int -> [Int]
aliquots n
  | n <= 1 = []
  | otherwise = findAliquots n 2 [n,1]

findAliquots :: Int -> Int -> [Int] -> [Int]
findAliquots n dividend output
  | dividend == n = output
  | (output !! 0 - output !! 1) <= 1 = output
  | isInt quotient = findAliquots n (dividend+1) ([quotient,dividend]++output)
  | otherwise = findAliquots n (dividend+1) output
  where quotient = n / dividend

isInt x = x == fromInteger (round x)
