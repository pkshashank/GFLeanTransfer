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
  let trees =  parse engGr eng (extract $ readType "text") s

  -- Giving names to all notions 
  let namedTrees = map changeMetaToInt trees
  let linNamedTrees = map (\x -> "Added variable names: " ++ linearize engGr eng x) namedTrees
  mapM_ putStrLn linNamedTrees

  -- simplification
  let simplifiedTrees = map (\x -> Texts.gf $ simplifyText (Texts.fg x :: Texts.Gtext)) namedTrees
  let linSimplifiedTrees = map (\x -> "Simplified: " ++ linearize engGr eng x) simplifiedTrees
  mapM_ putStrLn linSimplifiedTrees


  -- translating
  let translatedTrees = map (\x -> Blocks.gf $ translateText (Texts.fg x :: Texts.Gtext)) simplifiedTrees
  let rawOutputs = map (linearize leanGr lean) translatedTrees

  -- outputting after post-processing
  let linPostprocessed = map (\x -> "Translated: " ++ postprocess x) rawOutputs
  mapM_ putStrLn linPostprocessed
