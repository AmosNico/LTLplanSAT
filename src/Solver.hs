{-# LANGUAGE OverloadedStrings #-}

module Solver (Encoding (..), Options (..), solveSAS, solvePDDL, exampleRover, exampleAirport) where

import Basic (Action (..))
import Constraints (Constraints (minimalTimeLimit), NoConstraint (NoConstraint), Time, Variable)
import Control.Monad (void, when)
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map)
import qualified Ersatz as E
import ExistsStepSAT (existsStepEncoding, extractExistsStepPlan)
import LTLConstraints (pddlConstraintsToLTL)
import PDDLConstraints (selectSoftConstraints)
import ParsePDDLConstraints (parsePDDLConstraints)
import ParseSAS (readSAS)
import PlanningTask (PlanningTask (ptConstraints), fromSAS)
import SequentialSAT (Plan (..), extractSequentialPlan, sequentialEncoding)
import System.Process (callProcess, readProcess)

data Encoding = Sequential | ExistsStep

instance Show Encoding where
  show Sequential = "sequential"
  show ExistsStep = "exists-step"

data Options = Options
  { softConstraintsProbability :: Double,
    convertToLTL :: Bool,
    outputPlanningTask :: Bool,
    maxTimeSteps :: Int,
    encoding :: Encoding,
    optValidate :: Bool
  }

callSAT :: (Constraints c) => PlanningTask c -> Options -> Time -> IO (E.Result, Maybe (Map Variable Bool))
callSAT pt options k = do
  putStrLn $ "Initialize SAT-description with maximal plan length " ++ show k ++ "."
  let problem = case encoding options of
        Sequential -> sequentialEncoding pt k
        ExistsStep -> existsStepEncoding pt k
  putStrLn "Calling SAT-solver."
  E.solveWith E.cryptominisat5 problem

extractPlan :: Options -> PlanningTask c -> Time -> Map Variable Bool -> Plan
extractPlan options pt k v = case encoding options of
  Sequential -> extractSequentialPlan pt k v
  ExistsStep -> extractExistsStepPlan pt k v

iterativeSolve :: (Constraints c) => Options -> PlanningTask c -> Time -> IO Plan
iterativeSolve options pt k = do
  (res, mSolution) <- callSAT pt options k
  case res of
    E.Unsatisfied ->
      if k <= maxTimeSteps options
        then iterativeSolve options pt (ceiling (fromIntegral k * sqrt 2 :: Double))
        else
          fail $
            "Giving up. There exists no plan of length"
              ++ show (maxTimeSteps options)
              ++ " or less, the chosen constraints might be unsatisfiable."
    E.Unsolved -> fail "The SAT-solver could not solve the planning problem."
    E.Satisfied -> case mSolution of
      Nothing -> fail "The SAT-solver said the planning problem is solvable, but did not return a solution."
      Just solution -> return $ extractPlan options pt k solution

solve :: (Constraints c) => Options -> PlanningTask c -> IO Plan
solve options pt = iterativeSolve options pt $ minimalTimeLimit $ ptConstraints pt

solveSAS' :: (Constraints c) => Options -> c -> FilePath -> IO Plan
solveSAS' options constraints path = do
  sas <- readSAS path
  putStrLn "Translating to STRIPS."
  let pt = fromSAS sas constraints
  when (outputPlanningTask options) $ print pt
  plan <- solve options pt
  print plan
  return plan

solveSAS :: Options -> FilePath -> IO Plan
solveSAS options = solveSAS' options NoConstraint

writePlan :: Plan -> IO ()
writePlan (Plan as) =
  C8.writeFile "plan.txt" $
    C8.concat $
      map (\a -> C8.concat ["(", actionName a, ")\n"]) as

validatePlan :: FilePath -> FilePath -> Plan -> IO ()
validatePlan domain problem plan = do
  putStrLn "Checking the plan using Val."
  writePlan plan
  callProcess "Val/bin/validate.exe" [domain, problem, "plan.txt"]

solvePDDL :: Options -> FilePath -> FilePath -> IO ()
solvePDDL options domain problem = do
  putStrLn "Calling Fast-Downward to translate to SAS."
  void $ readProcess "python" ["fast-downward/src/translate/translate.py", "--keep-unimportant-variables", domain, problem] ""
  putStrLn "Translation to SAS succeded."
  constraints <- parsePDDLConstraints problem
  constraints' <- selectSoftConstraints constraints (softConstraintsProbability options)
  plan <-
    if convertToLTL options
      then solveSAS' options (pddlConstraintsToLTL constraints') "output.sas"
      else solveSAS' options constraints' "output.sas"
  when (optValidate options) $ validatePlan domain problem plan

exampleRover :: Options -> Int -> IO ()
exampleRover options n = solvePDDL options domain problem
  where
    version = if n < 10 then "0" ++ show n else show n
    domain = "examples-PDDL/IPC5-rovers/QualitativePreferences/domain.pddl"
    problem = "examples-PDDL/IPC5-rovers/QualitativePreferences/p" ++ version ++ ".pddl"

exampleAirport :: Options -> Int -> IO ()
exampleAirport options n = void $ solveSAS options ("examples-SAS/" ++ show n ++ ".in")