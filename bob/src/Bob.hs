module Bob (responseFor) where

import Data.Char (isUpper,toUpper,isSpace,isLetter)
import Data.List (dropWhileEnd)

responseFor :: String -> String
responseFor xs
  | trimed == "" = "Fine. Be that way!"
  | (isAllUpperStr trimed && endswith '?' xs) = "Calm down, I know what I'm doing!"
  | isAllUpperStr trimed = "Whoa, chill out!"
  | endswith '?' trimed = "Sure."
  | otherwise = "Whatever."
  where trimed = dropEndingSpaces xs

dropEndingSpaces :: String -> String
dropEndingSpaces = dropWhileEnd isSpace

isAllUpperStr :: String -> Bool
isAllUpperStr str = atLeastOneLetter str && isAllUpper str

isAllUpper :: String -> Bool
isAllUpper str = (map toUpper str) == str

atLeastOneLetter :: String -> Bool
atLeastOneLetter str = any isLetter str

endswith :: Char -> String -> Bool
endswith endChar str
  | last str  == endChar = True
  | otherwise = False
