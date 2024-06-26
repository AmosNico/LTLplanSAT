module Solver (solveSAS, solvePDDL, exampleRover, exampleAirport) where

import Constraints (Constraints, NoConstraint (NoConstraint), showConstraints)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as C8
import PDDLConstraints (selectSoftConstraints)
import ParsePDDLConstraints (parsePDDLConstraints)
import ParseSAS (readSAS)
import PlanningTask (fromSAS)
import SAT (solve)
import System.Process (callProcess, readProcess)
import Types (Options (..), Plan, writePlan)

solveSAS' :: (Constraints c) => Options -> c -> FilePath -> IO Plan
solveSAS' options constraints path = do
  sas <- readSAS path
  putStrLn "Translating to STRIPS."
  let pt = fromSAS sas constraints
  -- printPlanningTask pt
  plan <- solve options pt
  print plan
  return plan

solveSAS :: Options -> FilePath -> IO Plan
solveSAS options = solveSAS' options NoConstraint

validatePlan :: FilePath -> FilePath -> Plan -> IO ()
validatePlan domain problem plan = do
  putStrLn "Checking the plan using Val."
  writePlan plan
  callProcess "Val\\bin\\validate.exe" [domain, problem, "plan.txt"]

-- TODO constraints
solvePDDL :: Options -> FilePath -> FilePath -> IO ()
solvePDDL options domain problem = do
  putStrLn "Calling Fast-Downward to translate to SAS."
  void $ readProcess "python" ["src\\translate\\translate.py", "--keep-unimportant-variables", domain, problem] ""
  putStrLn "Translation to SAS succeded."
  constraints <- parsePDDLConstraints problem
  constraints' <- selectSoftConstraints constraints (softConstraintsProbability options)
  C8.putStrLn $ showConstraints constraints'
  plan <- solveSAS' options constraints' "output.sas"
  validatePlan domain problem plan

exampleRover :: Options -> Int -> IO ()
exampleRover options n = solvePDDL options domain problem
  where
    version = if n < 10 then "0" ++ show n else show n
    domain = "examples PDDL\\IPC5 - rovers\\QualitativePreferences\\domain.pddl"
    problem = "examples PDDL\\IPC5 - rovers\\QualitativePreferences\\p" ++ version ++ ".pddl"

exampleAirport :: Options -> Int -> IO ()
exampleAirport options n = void $ solveSAS options ("examples SAS\\" ++ show n ++ ".in")