module DNA (toRNA) where

toRNA :: String -> Maybe String
toRNA xs
  | xs == "" = Nothing
  | otherwise = Just $ toRNA' xs

toRNA' :: String -> String
toRNA' = map compl

compl :: Char -> Char
compl dna
  | dna == 'G' = 'C'
  | dna == 'C' = 'G'
  | dna == 'T' = 'A'
  | dna == 'A' = 'U'
