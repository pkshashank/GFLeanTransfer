module Processing where
import Data.Char (toLower)

-- changes "A1s." to "a1s ."
preprocess :: String -> String
preprocess = replaceChar ',' " ," . replaceChar '.' " ." . map toLower

-- relace char with a string in a string
replaceChar :: Char -> String -> String -> String
replaceChar x y [] = []
replaceChar x y (z : ls) =  if x == z
                                then   y ++ replaceChar x y ls
                            else 
                                z : replaceChar x y ls