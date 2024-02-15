import PGF
import Texts -- English file
import Blocks -- Lean grammar
import GF.Support
import Data.Maybe
import Simplifications
import Translations
import Utilities
import Prelude
import Processing

main :: IO()
main = do
  engGr <- readPGF "Texts.pgf"
  leanGr <- readPGF "Blocks.pgf"
  let eng = head $ languages engGr
  let lean = head $ languages leanGr
  x <- readFile "prompt.txt"
  let s = preprocess x
  putStr ("Preprocessing: " ++ s) 
  let tree = head $ parse engGr eng (extract $ readType "text") s

  -- Giving names to all notions 
  let namedTree = changeMetaToInt tree
  putStrLn ("Added variable names: " ++ linearize engGr eng namedTree)

  -- simplification
  let simplifiedTree = Texts.gf $ simplifyText (Texts.fg namedTree :: Texts.Gtext)
  putStrLn ("Simplified: " ++ linearize engGr eng simplifiedTree)

  -- translating
  let translatedTree = Blocks.gf $ translateText (Texts.fg simplifiedTree :: Texts.Gtext)
  let rawOutput = linearize leanGr lean translatedTree

  -- outputting after post-processing
  putStrLn ("Translated: "++ postprocess rawOutput)
  return ()
