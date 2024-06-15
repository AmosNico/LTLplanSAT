{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import ParseSAS (readSAS)
import STRIPS (fromFDR)
import SAT (solve)

import System.Environment (getArgs)
import System.Process (readProcess)
import Text.Read (readMaybe)
import Control.Monad (unless, void) 
import Control.Exception (catch, SomeException)

solveSAS :: FilePath -> IO ()
solveSAS path = do
  fdr <- readSAS path
  putStrLn "Translating to STRIPS."
  plan <- solve $ fromFDR fdr
  print plan
  --ltl <-  parseLTL problem
  --print ltl

solvePDDL :: FilePath -> FilePath -> IO ()
solvePDDL domain problem = do
  putStrLn "Calling Fast-Downward to translate to SAS."
  void $ readProcess "python" ["src\\translate\\translate.py","--keep-unimportant-variables", domain, problem] ""
  putStrLn "Translation to SAS succeded."
  solveSAS "output.sas"

exampleRover :: Int -> IO ()
exampleRover n = solvePDDL
  "examples PDDL\\IPC5 - rovers\\QualitativePreferences\\domain.pddl"
  ("examples PDDL\\IPC5 - rovers\\QualitativePreferences\\p" ++ version ++ ".pddl") where
    version = if n < 10 then "0" ++ show n else show n

exampleAirport :: Int -> IO ()
exampleAirport n = solveSAS $ "examples SAS\\" ++ show n ++ ".in"

description :: String
description = "\nYou can call the solver immediately by providing arguments with the "
  ++ "path of the domain file and the path of the problem file.\n"
  ++ "Alternatively, you can run one of the following commands (WARNING: currently can't handle spaces in paths):\n"
  ++ "* \"pddl domain problem\" where domain is the path of the domain file "
  ++ "and problem is the path of the problem file. This runs the solver on the specified files.\n"
  ++ "* \"rovers n\" where n is a number between 1 and 20. "
  ++ "This runs the solver on the rovers domain with the problem file corresponding to n.\n"
  ++ "* \"sas problem\" where problem is the path of a SAS-file. "
  ++ "This runs the solver on the specified file.\n"
  ++ "* \"airport n\" where n is a number between 1 and 4. "
  ++ "This runs the solver on the corresponding sas-file in the folder \"examples SAS\"" 
  ++ "* \"help\" to display this description.\n"
  ++ "* \"quit\" to stop the program.\n"

readInRange :: Int -> Int -> String -> Maybe Int
readInRange l u s = do
  n <- readMaybe s
  if l <= n && n <= u
    then Just n
    else Nothing

-- TODO: can't handle spaces in filenames
handleCommand :: String -> IO ()
handleCommand command = case words command of
    ["pddl", domain, problem] -> solvePDDL domain problem
    ["rovers", s] -> case readInRange 1 20 s of
      Nothing -> putStrLn $
        "Expected an integer between 1 and 20, but got " ++ s ++ "."
      Just n -> exampleRover n
    ["sas", path] -> solveSAS path
    ["airport", s] -> case readInRange 1 4 s of
      Nothing -> putStrLn $
        "Expected an integer between 1 and 4, but got " ++ s ++ "."
      Just n -> exampleAirport n
    ["help"] -> putStrLn description
    ["quit"] -> return ()
    _ -> do
      putStrLn $ "Command " ++ show command ++ " not recognised."

loop :: IO ()
loop = do
  command <- getLine
  _ <- catch (handleCommand command) (\e -> print (e :: SomeException))
  unless (command == "quit") loop

main :: IO ()
main = do
  args <- getArgs
  case args of
    [domain, problem] -> solvePDDL domain problem
    _ -> do
      putStrLn "Welcome to LTLplanSAT"
      putStrLn description
      loop