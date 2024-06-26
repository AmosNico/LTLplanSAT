{-# LANGUAGE TupleSections #-}

module SAT (solve) where

import Constraints (Constraints (constraintsToSAT), atMostOne, value)
import Control.Monad.State.Lazy (StateT)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Ersatz as E
import ExistsStepSAT (existsStep, extractExistsStepPlan)
import PlanningTask
import Types

defineVariables :: (E.MonadSAT s m) => PlanningTask c -> Time -> m (Map Variable E.Bit)
defineVariables pt k = sequence $ Map.fromList $ map (,E.exists) (actionVars ++ atomVars)
  where
    actionVars = [ActionV t a | a <- ptActions pt, t <- [1 .. k]]
    atomVars = [AtomV t atom | atom <- ptAtoms pt, t <- [0 .. k]]

initialToSAT :: PlanningTask c -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ map (value v 0) $ ptInitalState pt

goalToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (value v k) $ ptGoal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add
  where
    pre = map (\f -> v ! ActionV t a E.==> value v (t - 1) f) $ Set.toList $ actionPre a
    add = map (\f -> v ! ActionV t a E.==> value v t f) $ Set.toList $ actionPost a

frameAxiom :: PlanningTask c -> Fact -> Time -> Map Variable E.Bit -> E.Bit
frameAxiom pt fact t v = E.or $ [value v (t - 1) fact, value v t $ negateFact fact] ++ actions
  where
    actions = map (\a -> v ! ActionV t a) $ filter (\a -> fact `elem` actionPost a) $ ptActions pt

-- k is the maximum number of timesteps in the SAT encoding
ptToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
ptToSAT pt k v =
  E.and
    [ initialToSAT pt v,
      goalToSAT pt k v,
      E.and [actionToSAT action t v | action <- ptActions pt, t <- [1 .. k]],
      E.and [frameAxiom pt fact t v | fact <- ptFacts pt, t <- [1 .. k]]
    ]

mutexesToSAT :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
mutexesToSAT pt t v = mapM_ mutexToSAT (ptMutexGroups pt)
  where
    mutexToSAT (MutexGroup facts) = atMostOne $ map (value v t) facts

noParallelActions :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
noParallelActions pt k v = sequence_ [atMostOneAction t | t <- [1 .. k]]
  where
    atMostOneAction t = atMostOne $ map (\a -> v ! ActionV t a) $ ptActions pt

initializeProblem :: (Constraints c) => PlanningTask c -> Options -> Time -> StateT E.SAT IO (Map Variable E.Bit)
initializeProblem pt options k = do
  vars <- defineVariables pt k
  E.assert $ ptToSAT pt k vars
  case encoding options of
    Sequential -> noParallelActions pt k vars
    ExistsStep -> existsStep pt k vars
  mutexesToSAT pt k vars
  constraintsToSAT (ptConstraints pt) k vars
  return vars

callSAT :: (Constraints c) => PlanningTask c -> Options -> Time -> IO (E.Result, Maybe (Map Variable Bool))
callSAT pt options k = do
  putStrLn $ "Initialize SAT-description with maximal plan length " ++ show k ++ "."
  let problem = initializeProblem pt options k
  putStrLn "Calling SAT-solver."
  E.solveWith E.cryptominisat5 problem

extractSequentialPlan :: PlanningTask c -> Time -> Map Variable Bool -> Plan
extractSequentialPlan pt k v = Plan $ mapMaybe extractAction [1 .. k]
  where
    extractAction :: Time -> Maybe Action
    extractAction t = case filter (\a -> v ! ActionV t a) $ ptActions pt of
      [] -> Nothing
      [a] -> Just a
      l ->
        error $
          "The solution contains "
            ++ show (length l)
            ++ " actions at the same time, "
            ++ "namely "
            ++ show l
            ++ "."

extractPlan :: PlanningTask c -> Options -> Time -> Map Variable Bool -> Plan
extractPlan pt options k v = case encoding options of
  Sequential -> extractSequentialPlan pt k v
  ExistsStep -> extractExistsStepPlan pt k v

iterativeSolve :: (Constraints c) => PlanningTask c -> Options -> Time -> IO Plan
iterativeSolve pt options k = do
  (res, mSolution) <- callSAT pt options k
  case res of
    E.Unsatisfied ->
      if k <= maxTimeSteps options
        then iterativeSolve pt options (ceiling (fromIntegral k * sqrt 2 :: Double))
        else
          fail $
            "Giving up. There exists no plan of length"
              ++ show (maxTimeSteps options)
              ++ " or less, the chosen constraints might be unsatisfiable."
    E.Unsolved -> fail "The SAT-solver could not solve the planning problem."
    E.Satisfied -> case mSolution of
      Nothing -> fail "The SAT-solver said the planning problem is solvable, but did not return a solution."
      Just solution -> return $ extractPlan pt options k solution

solve :: (Constraints c) => PlanningTask c -> Options -> IO Plan
solve pt options = do
  iterativeSolve pt options 5