{-# LANGUAGE TupleSections #-}

module SAT (solve) where

import Control.Monad.State.Lazy (StateT)
import Constraints (Constraints (constraintsToSAT), atMostOne, value)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Ersatz as E
import PlanningTask
import Types

defineVariables :: (E.MonadSAT s m) => PlanningTask c -> Time -> m (Map Variable E.Bit)
defineVariables pt k = sequence $ Map.fromList $ map (,E.exists) (actionVars ++ factVars)
  where
    actionVars = [ActionV t a | a <- ptActions pt, t <- [1 .. k]]
    factVars = [AtomV t atom | atom <- ptAtoms pt, t <- [0 .. k]]

initialToSAT :: PlanningTask c -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ map (value v 0) $ ptInitalState pt

goalToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (value v k) $ ptGoal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add
  where
    pre = map (\f -> v ! ActionV t a E.==> value v (t - 1) f) $ actionPre a
    add = map (\f -> v ! ActionV t a E.==> value v t f) $ actionPost a

frameAxioms :: PlanningTask c -> Atom -> Time -> Map Variable E.Bit -> E.Bit
frameAxioms pt atom t v = frame1 E.&& frame2
  where
    fInAdd = filter (\a -> PosAtom atom `elem` actionPost a) $ ptActions pt
    frame1 = E.or $ [value v (t - 1) $ PosAtom atom, value v t $ NegAtom atom] ++ map (\a -> v ! ActionV t a) fInAdd
    fInDel = filter (\a -> NegAtom atom `elem` actionPost a) $ ptActions pt
    frame2 = E.or $ [value v (t - 1) $ NegAtom atom, value v t $ PosAtom atom] ++ map (\a -> v ! ActionV t a) fInDel

-- k is the maximum number of timesteps in the SAT encoding
ptToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
ptToSAT pt k v =
  E.and
    [ initialToSAT pt v,
      goalToSAT pt k v,
      E.and [actionToSAT a t v | a <- ptActions pt, t <- [1 .. k]],
      E.and [frameAxioms pt atom t v | atom <- ptAtoms pt, t <- [1 .. k]]
    ]

mutexesToSAT :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
mutexesToSAT pt t v = mapM_ mutexToSAT (ptMutexGroups pt) where
  mutexToSAT (MutexGroup facts) = atMostOne $ map (value v t) facts

noParallelActions :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
noParallelActions pt k v = sequence_  [atMostOneAction t | t <- [1..k]] where
  atMostOneAction t = atMostOne $ map (\a -> v ! ActionV t a) $ ptActions pt

initializeProblem :: (Constraints c) => PlanningTask c -> Time -> StateT E.SAT IO (Map Variable E.Bit)
initializeProblem pt k = do
  vars <- defineVariables pt k
  E.assert $ ptToSAT pt k vars
  noParallelActions pt k vars
  mutexesToSAT pt k vars
  constraintsToSAT (ptConstraints pt) k vars
  return vars

callSAT :: (Constraints c) => PlanningTask c -> Time -> IO (E.Result, Maybe (Map Variable Bool))
callSAT pt k = do
  putStrLn $ "Initialize SAT-description with maximal plan length " ++ show k ++ "."
  let problem = initializeProblem pt k
  putStrLn "Calling SAT-solver."
  E.solveWith E.cryptominisat5 problem

extractPlan :: PlanningTask c -> Time -> Map Variable Bool -> Plan
extractPlan pt k v = Plan $ mapMaybe extractAction [1 .. k]
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

iterativeSolve :: (Constraints c) => PlanningTask c -> Time -> IO Plan
iterativeSolve pt k = do
  (res, mSolution) <- callSAT pt k
  case res of
    E.Unsatisfied ->
      if k <= 50
        then iterativeSolve pt (ceiling (fromIntegral k * sqrt 2 :: Double))
        else fail "Giving up. There exists no plan of length 50 or less, the chosen constraints might be unsatisfiable."
    E.Unsolved -> fail "The SAT-solver could not solve the planning problem."
    E.Satisfied -> case mSolution of
      Nothing -> fail "The SAT-solver said the planning problem is solvable, but did not return a solution."
      Just solution -> return $ extractPlan pt k solution

solve :: (Constraints c) => PlanningTask c -> IO Plan
solve pt = do
  iterativeSolve pt 5