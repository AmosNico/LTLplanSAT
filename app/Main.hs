module Main (main) where

import ParseSAS (readSAS)
import STRIPS (fromFDR, printSTRIPS)
import ParseLTL (parseLTL)
import System.Environment (getArgs)
import System.Process (callCommand)

getInputFiles :: IO (String, String)
getInputFiles = do
  args <- getArgs
  case args of
    [domain, problem] -> return (domain, problem)
    _ -> error 
      "Please specify the domain and program files as arguments of the program."

translateToSAS :: String -> String -> IO ()
translateToSAS domain problem = do
  callCommand $
    "python \"src\\translate\\translate.py\" --keep-unimportant-variables " ++ 
    show domain ++ " " ++ show problem

main :: IO ()
main = do
  (domain, problem) <- getInputFiles
  -- translateToSAS domain problem
  -- fdr <- readSAS
  ltl <-  parseLTL problem
  print ltl
  -- printSTRIPS (fromFDR fdr)
