{-# LANGUAGE TupleSections #-}

module SequentialSAT (basicSATEncoding, sequentialEncoding, Plan (..), extractSequentialPlan) where

import Constraints (Constraints (constraintsToSAT), atMostOne, value)
import Control.Monad.State.Lazy (StateT)
import Data.List (intercalate)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Ersatz as E
import PlanningTask
import Basic

newtype Plan = Plan [Action]

instance Show Plan where
  show (Plan as) =
    "Plan with length "
      ++ show (length as)
      ++ " and cost "
      ++ show (sum $ map actionCost as)
      ++ ":\n  "
      ++ intercalate "\n  " (map show as)

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
basicSATEncoding :: PlanningTask c -> Time -> StateT E.SAT IO (Map Variable E.Bit)
basicSATEncoding pt k = do
  v <- defineVariables pt k
  E.assert $ initialToSAT pt v
  E.assert $ goalToSAT pt k v
  E.assert $ E.and [actionToSAT action t v | action <- ptActions pt, t <- [1 .. k]]
  E.assert $ E.and [frameAxiom pt fact t v | fact <- ptFacts pt, t <- [1 .. k]]
  return v

mutexesToSAT :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
mutexesToSAT pt t v = mapM_ mutexToSAT (ptMutexGroups pt)
  where
    mutexToSAT (MutexGroup facts) = atMostOne $ map (value v t) facts

noParallelActions :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
noParallelActions pt k v = sequence_ [atMostOneAction t | t <- [1 .. k]]
  where
    atMostOneAction t = atMostOne $ map (\a -> v ! ActionV t a) $ ptActions pt

sequentialEncoding :: (Constraints c) => PlanningTask c -> Time -> StateT E.SAT IO (Map Variable E.Bit)
sequentialEncoding pt k = do
  vars <- basicSATEncoding pt k
  noParallelActions pt k vars
  mutexesToSAT pt k vars
  constraintsToSAT (ptConstraints pt) k vars
  return vars

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