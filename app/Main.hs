module Main (main) where

import ParseSAS (readSAS)
import STRIPS (fromFDR, printSTRIPS)

main :: IO ()
main = do
  fdr <- readSAS
  let strips = fromFDR fdr
  printSTRIPS strips
