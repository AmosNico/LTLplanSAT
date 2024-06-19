{-# LANGUAGE TupleSections #-}

module SAT (solve) where

import Constraints (Constraints (constraintsToSat), atMostOne, atMostOneVariables, value)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Ersatz as E
import PlanningTask
import Types

defineVariables :: (E.MonadSAT s m) => PlanningTask c -> Time -> m (Map Variable E.Bit)
defineVariables pt k = sequence $ Map.fromList $ map (,E.exists) (actionVars ++ factVars ++ amoVars)
  where
    actionVars = [ActionV t a | a <- ptActions pt, t <- [1 .. k]]
    factVars = [AtomV t atom | atom <- ptAtoms pt, t <- [0 .. k]]
    amoVars = concat [atMostOneVariables (AtMostOneActionV t) (ptNumberActions pt) | t <- [0 .. k]]

initialToSAT :: PlanningTask c -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ map (value v 0) $ ptInitalState pt

goalToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (value v k) $ ptGoal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add
  where
    pre = map (\f -> v ! ActionV t a E.==> value v (t - 1) f) $ actionPre a
    add = map (\f -> v ! ActionV t a E.==> value v t f) $ actionPost a

-- TODO: Mutexes, not invariants
mutexToSAT :: MutexGroup -> Time -> Map Variable E.Bit -> E.Bit
mutexToSAT (MutexGroup facts) t v = E.true -- E.or $ map (value v t) facts

frameAxioms :: PlanningTask c -> Atom -> Time -> Map Variable E.Bit -> E.Bit
frameAxioms pt atom t v = frame1 E.&& frame2
  where
    fInAdd = filter (\a -> PosAtom atom `elem` actionPost a) $ ptActions pt
    frame1 = E.or $ [value v (t - 1) $ PosAtom atom, value v t $ NegAtom atom] ++ map (\f' -> v ! ActionV t f') fInAdd
    fInDel = filter (\a -> NegAtom atom `elem` actionPost a) $ ptActions pt
    frame2 = E.or $ [value v (t - 1) $ NegAtom atom, value v t $ PosAtom atom] ++ map (\f' -> v ! ActionV t f') fInDel

-- k is the maximum number of timesteps in the SAT encoding
ptToSAT :: (Constraints c) => PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
ptToSAT pt k v =
  E.and
    [ initialToSAT pt v,
      goalToSAT pt k v,
      E.and [actionToSAT a t v | a <- ptActions pt, t <- [1 .. k]],
      E.and [frameAxioms pt atom t v | atom <- ptAtoms pt, t <- [1 .. k]],
      constraintsToSat (ptConstraints pt) k v,
      E.and [mutexToSAT mg t v | mg <- ptMutexGroups pt, t <- [0 .. k]],
      E.and [atMostOne (AtMostOneActionV t) [ActionV t a | a <- ptActions pt] v | t <- [1 .. k]]
    ]

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

callSAT :: (Constraints c) => PlanningTask c -> Time -> IO (E.Result, Maybe (Map Variable Bool))
callSAT pt k = E.solveWith E.cryptominisat5 $ do
  vars <- defineVariables pt k
  E.assert $ ptToSAT pt k vars
  return vars

iterativeSolve :: (Constraints c) => PlanningTask c -> Time -> IO Plan
iterativeSolve pt k = do
  putStrLn $ "Calling the SAT-solver with maximal plan length " ++ show k ++ "."
  (res, mSolution) <- callSAT pt k
  case res of
    E.Unsatisfied -> iterativeSolve pt (ceiling (fromIntegral k * sqrt 2 :: Double))
    E.Unsolved -> error "The SAT-solver could not solve the planning problem."
    E.Satisfied -> case mSolution of
      Nothing -> error "The SAT-solver said the planning problem is solvable, but did not return a solution."
      Just solution -> return $ extractPlan pt k solution

solve :: (Constraints c) => PlanningTask c -> IO Plan
solve pt = do
  iterativeSolve pt 5