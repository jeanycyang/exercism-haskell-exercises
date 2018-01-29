module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n < 0 = Nothing
  | otherwise = Just $ fst $ collatz' (0, n)

collatz' :: (Integer, Integer) -> (Integer, Integer)
collatz' (count, n)
  | n == 1 = (count, 1)
  | even n = collatz' (count + 1, n `div` 2)
  | otherwise = collatz' (count + 1, 3 * n + 1)
