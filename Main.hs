import PGF
import Texts -- English file
import Blocks -- Lean grammar
import GF.Support
import Data.Maybe
import Simplifications
import Translations
import Utilities
import Prelude
import Preprocessing

main :: IO()
main = do
  engGr <- readPGF "Texts.pgf"
  leanGr <- readPGF "Blocks.pgf"
  let eng = head $ languages engGr
  let lean = head $ languages leanGr
  x <- readFile "prompt.txt"
  let s = preprocess x
  putStrLn ("Preprocessing:" ++ s) 
  let tree = head $ parse engGr eng (extract $ readType "text") s

  -- Giving names to all notions 
  let namedTree = changeMetaToInt tree
  putStrLn ("added variable names - " ++ linearize engGr eng namedTree)

  -- simplification
  let simplifiedTree = Texts.gf $ simplifyText (Texts.fg namedTree :: Texts.Gtext)
  putStrLn ("simplified - " ++ linearize engGr eng simplifiedTree)

  -- translating
  let translatedTree = Blocks.gf $ translateText (Texts.fg simplifiedTree :: Texts.Gtext)
  putStrLn ("translated - "++ linearize leanGr lean translatedTree)
  return ()