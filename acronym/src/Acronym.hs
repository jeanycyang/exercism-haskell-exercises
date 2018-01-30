module Acronym (abbreviate) where

import Data.Char (toUpper)
import Text.Regex.Posix

abbreviate :: String -> String
abbreviate xs = toUpperStr [firstLetter | (firstLetter:_) <- (words xs)]

toUpperStr str = map toUpper str


