module Processing where
import Data.Char (toLower)
import Data.List
import Utilities

-- changes "A1s." to "a1s ."
preprocess :: String -> String
preprocess = replaceString "," " ," . replaceString "." " ." . map toLower


postprocess :: String -> String
postprocess = changeQM . replaceString " &+ " ""

-- Replace string `a` with `b` in a string `s`
replaceString :: String -> String -> String -> String
replaceString a b s@(x:xs) = if a `isPrefixOf` s
                                then b ++ replaceString a b (drop (length a) s)
                            else x : replaceString a b xs

replaceString _ _ [] = []


changeQM :: String -> String
changeQM s@(x : xs) =  if x == '?'
                        then show (length s) ++ changeQM xs
                        else x : changeQM xs
changeQM [] = []