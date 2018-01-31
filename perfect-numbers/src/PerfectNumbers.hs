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
  | dividend `elem` output = output
  | remainder == 0 = findAliquots n (dividend+1) (newAliquots++output)
  | otherwise = findAliquots n (dividend+1) output
  where remainder = n `mod` dividend
        quotient = n `div` dividend
        newAliquots = if dividend == quotient then [quotient] else [quotient,dividend]

