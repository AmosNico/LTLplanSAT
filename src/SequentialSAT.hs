{-# LANGUAGE TupleSections #-}

module SequentialSAT (basicSATEncoding, sequentialEncoding, Plan (..), extractSequentialPlan) where

import Basic (Action (..), Fact, MutexGroup (..), negateFact, showActionName)
import Constraints (IsConstraints (constraintsToSAT), Time, Variable (..), atLeastOneAction, atMostOne, atMostOneAction, conditionalAtMostOne, noAction, value)
import Control.Monad.State.Lazy (StateT)
import Data.List (intercalate)
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Ersatz as E
import PlanningTask (PlanningTask (..), ptFacts)

newtype Plan = Plan [Action]

instance Show Plan where
  show (Plan as) =
    "Plan with length "
      ++ show (length as)
      ++ " and cost "
      ++ show (sum $ map actionCost as)
      ++ ":\n  "
      ++ intercalate "\n  " (map showActionName as)

defineVariables :: (E.MonadSAT s m) => PlanningTask c -> Time -> m (Map Variable E.Bit)
defineVariables pt k = sequence $ Map.fromList $ map (,E.exists) vars
  where
    vars = actionVars ++ atomVars ++ amoVars ++ aloVars ++ noaVars
    actionVars = [ActionVar t a | a <- ptActions pt, t <- [1 .. k]]
    atomVars = [AtomVar t atom | atom <- ptAtoms pt, t <- [0 .. k]]
    amoVars = [AtMostOneAction t | t <- [1 .. k]]
    aloVars = [AtLeastOneAction t | t <- [1 .. k]]
    noaVars = [NoAction t | t <- [1 .. k]]

initialToSAT :: PlanningTask c -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ map (value v 0) $ ptInitalState pt

goalToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (value v k) $ ptGoal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add
  where
    pre = map (\f -> v ! ActionVar t a E.==> value v (t - 1) f) $ Set.toList $ actionPre a
    add = map (\f -> v ! ActionVar t a E.==> value v t f) $ Set.toList $ actionPost a

frameAxiom :: PlanningTask c -> Fact -> Time -> Map Variable E.Bit -> E.Bit
frameAxiom pt fact t v = E.or $ [value v (t - 1) fact, value v t $ negateFact fact] ++ actions
  where
    actions = map (\a -> v ! ActionVar t a) $ filter (\a -> fact `elem` actionPost a) $ ptActions pt

mutexesToSAT :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
mutexesToSAT pt t v = mapM_ mutexToSAT (ptMutexGroups pt)
  where
    mutexToSAT (MutexGroup facts) = atMostOne $ map (value v t) facts

-- TODO: We don't need to do these if there are no constraints
actionCountToSAT :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
actionCountToSAT pt k v = do
  let actionBits t = map (\a -> v ! ActionVar t a) $ ptActions pt
  let amoAction t = conditionalAtMostOne (atMostOneAction v t) (actionBits t)
  sequence_ [amoAction t | t <- [1 .. k]]
  let aloAction t = atLeastOneAction v t E.==> E.or (actionBits t)
  E.assert $ E.and [aloAction t | t <- [1 .. k]]
  E.assert $ E.and [noAction v t E.==> E.not aBit | t <- [1 .. k], aBit <- actionBits t]

-- k is the maximum number of timesteps in the SAT encoding
basicSATEncoding :: PlanningTask c -> Time -> StateT E.SAT IO (Map Variable E.Bit)
basicSATEncoding pt k = do
  v <- defineVariables pt k
  E.assert $ initialToSAT pt v
  E.assert $ goalToSAT pt k v
  E.assert $ E.and [actionToSAT action t v | action <- ptActions pt, t <- [1 .. k]]
  E.assert $ E.and [frameAxiom pt fact t v | fact <- ptFacts pt, t <- [1 .. k]]
  mutexesToSAT pt k v
  actionCountToSAT pt k v
  return v

sequentialEncoding :: (IsConstraints c) => PlanningTask c -> Time -> StateT E.SAT IO (Map Variable E.Bit)
sequentialEncoding pt k = do
  v <- basicSATEncoding pt k
  E.assert $ E.and $ map (atMostOneAction v) [1 .. k]
  constraintsToSAT (ptConstraints pt) k v
  return v

extractSequentialPlan :: PlanningTask c -> Time -> Map Variable Bool -> Plan
extractSequentialPlan pt k v = Plan $ mapMaybe extractAction [1 .. k]
  where
    extractAction :: Time -> Maybe Action
    extractAction t = case filter (\a -> v ! ActionVar t a) $ ptActions pt of
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