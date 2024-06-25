{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Constraints (Constraints (showConstraints), NoConstraint (..))
import Control.Exception (SomeException, catch)
import Control.Monad (unless, void)
import qualified Data.ByteString.Char8 as C8
import PDDLConstraints (selectSoftConstraints)
import ParsePDDLConstraints (parsePDDLConstraints)
import ParseSAS (readSAS)
import PlanningTask (fromSAS, printPlanningTask)
import SAT (solve)
import System.Environment (getArgs)
import System.Process (callProcess, readProcess)
import Text.Read (readMaybe)
import Types (Plan, writePlan, Options(..), Encoding (..))

solveSAS :: (Constraints c) => FilePath -> c -> IO Plan
solveSAS path constraints = do
  sas <- readSAS path
  putStrLn "Translating to STRIPS."
  let pt = fromSAS sas constraints
  -- printPlanningTask pt
  plan <- solve pt $ Options 0 ExistsStep
  print plan
  return plan

validatePlan :: FilePath -> FilePath -> Plan -> IO ()
validatePlan domain problem plan = do
  putStrLn "Checking the plan using Val."
  writePlan plan
  callProcess "Val\\bin\\validate.exe" [domain, problem, "plan.txt"]

solvePDDL :: FilePath -> FilePath -> IO ()
solvePDDL domain problem = do
  putStrLn "Calling Fast-Downward to translate to SAS."
  void $ readProcess "python" ["src\\translate\\translate.py", "--keep-unimportant-variables", domain, problem] ""
  putStrLn "Translation to SAS succeded."
  constraints <- parsePDDLConstraints problem
  constraints' <- selectSoftConstraints constraints 0.2
  C8.putStrLn $ showConstraints constraints'
  plan <- solveSAS "output.sas" constraints'
  validatePlan domain problem plan

exampleRover :: Int -> IO ()
exampleRover n =
  solvePDDL
    "examples PDDL\\IPC5 - rovers\\QualitativePreferences\\domain.pddl"
    ("examples PDDL\\IPC5 - rovers\\QualitativePreferences\\p" ++ version ++ ".pddl")
  where
    version = if n < 10 then "0" ++ show n else show n

exampleAirport :: Int -> IO ()
exampleAirport n = void $ solveSAS ("examples SAS\\" ++ show n ++ ".in") NoConstraint

description :: String
description =
  "\nYou can call the solver immediately by providing arguments with the "
    ++ "path of the domain file and the path of the problem file.\n"
    ++ "Alternatively, you can run one of the following commands:\n"
    ++ "* \"pddl domain problem\" where domain is the path of the domain file "
    ++ "and problem is the path of the problem file. This runs the solver on the specified files. "
    ++ "(WARNING: currently can't handle spaces in paths)\n"
    ++ "* \"rovers n\" where n is a number between 1 and 20. "
    ++ "This runs the solver on the rovers domain with the problem file corresponding to n.\n"
    ++ "* \"sas problem\" where problem is the path of a SAS-file. "
    ++ "This runs the solver on the specified file. "
    ++ "(WARNING: currently can't handle spaces in paths)\n"
    ++ "* \"airport n\" where n is a number between 1 and 4. "
    ++ "This runs the solver on the corresponding sas-file in the folder \"examples SAS\"\n"
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
    Nothing ->
      putStrLn $
        "Expected an integer between 1 and 20, but got " ++ s ++ "."
    Just n -> exampleRover n
  ["sas", path] -> void $ solveSAS path NoConstraint
  ["airport", s] -> case readInRange 1 4 s of
    Nothing ->
      putStrLn $
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
    [domain, problem] -> void $ solvePDDL domain problem
    _ -> do
      putStrLn "Welcome to LTLplanSAT"
      putStrLn description
      loop