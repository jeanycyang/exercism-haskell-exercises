module Acronym (abbreviate) where

import Data.Char (toUpper)
import Text.Regex.Posix

abbreviate :: String -> String
abbreviate xs = toUpperStr $
                  concat $
                  [getAbbrFromWord x | x <- (words xs)]

getAbbrFromWord :: String -> String
getAbbrFromWord str =
  let camelCased = str =~ "[a-z]+([A-Z]{1})" :: String
      hyphenated = str =~ "-[a-zA-Z]{1}" :: String
  in [head str] ++ getLast camelCased ++ getLast hyphenated

getLast :: String -> String
getLast "" = ""
getLast str = [last str]

toUpperStr str = map toUpper str


