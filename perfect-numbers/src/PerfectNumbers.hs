module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | n == 1 = Just Deficient
  | aliquotsSum > n = Just Abundant
  | aliquotsSum == n = Just Perfect
  | aliquotsSum < n = Just Deficient
  where aliquotsSum = sum $ aliquots n

aliquots :: Int -> [Int]
aliquots n
  | n <= 1 = []
  | otherwise = [1,n]

