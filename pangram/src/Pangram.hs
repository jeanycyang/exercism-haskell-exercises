module Pangram (isPangram) where

import Data.Char (isLetter,toLower)
import Data.Set (Set,fromList,size)

isPangram :: String -> Bool
isPangram text = size (fromList letters) == 26
  where letters = (removeNonLetters . makeStrCaseInsensitive) text

removeNonLetters :: String -> String
removeNonLetters = filter isLetter

makeStrCaseInsensitive :: String -> String
makeStrCaseInsensitive = map toLower
