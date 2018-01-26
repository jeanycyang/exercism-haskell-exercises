module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year
  | year <= 0 = error "year must be greater than 0"
  | (year `mod` 4) /= 0 = False
  | (year `mod` 100) /= 0 = True
  | (year `mod` 400) == 0 = True
  | otherwise = False
