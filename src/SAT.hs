{-# LANGUAGE TupleSections #-}

module SAT (solve) where

import Constraints (Constraints (constraintsToSat), atMostOne, atMostOneVariables)
import Data.List ((\\))
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
    factVars = [FactV t f | f <- ptFacts pt, t <- [0 .. k]]
    amoVars = concat [atMostOneVariables (AtMostOneActionV t) (ptNumberActions pt) | t <- [0 .. k]]

initialToSAT :: PlanningTask c -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ pos ++ neg
  where
    pos = map (\f -> v ! FactV 0 f) $ ptInitalState pt
    neg = map (\f -> E.not $ v ! FactV 0 f) $ ptFacts pt \\ ptInitalState pt

goalToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (\f -> v ! FactV k f) $ ptGoal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add ++ del
  where
    pre = map (\f -> v ! ActionV t a E.==> v ! FactV (t - 1) f) $ actionPre a
    add = map (\f -> v ! ActionV t a E.==> v ! FactV t f) $ actionAdd a
    del = map (\f -> v ! ActionV t a E.==> E.not (v ! FactV t f)) $ actionDel a

mutexToSAT :: MutexGroup -> Time -> Map Variable E.Bit -> E.Bit
mutexToSAT (MutexGroup facts) t v = E.or $ map (\f -> v ! FactV t f) facts

frameAxioms :: PlanningTask c -> Fact -> Time -> Map Variable E.Bit -> E.Bit
frameAxioms pt f t v = frame1 E.&& frame2
  where
    fInAdd = filter (\a -> f `elem` actionAdd a) $ ptActions pt
    frame1 = E.or $ [v ! FactV (t - 1) f, E.not $ v ! FactV t f] ++ map (\f' -> v ! ActionV t f') fInAdd
    fInDel = filter (\a -> f `elem` actionDel a) $ ptActions pt
    frame2 = E.or $ [E.not $ v ! FactV (t - 1) f, v ! FactV t f] ++ map (\f' -> v ! ActionV t f') fInDel

-- k is the maximum number of timesteps in the SAT encoding
ptToSAT :: (Constraints c) => PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
ptToSAT pt k v =
  E.and
    [ initialToSAT pt v,
      goalToSAT pt k v,
      E.and [actionToSAT a t v | a <- ptActions pt, t <- [1 .. k]],
      E.and [frameAxioms pt f t v | f <- ptFacts pt, t <- [1 .. k]],
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