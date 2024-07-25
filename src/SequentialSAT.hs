module SequentialSAT (basicSATEncoding, sequentialEncoding, extractSequentialPlan) where

import Constraints (IsConstraints, constraintsToSAT)
import Control.Monad.State.Lazy (StateT)
import Data.Map (Map)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Ersatz as E
import PlanningTask (Action (..), Fact, PlanningTask (..), goalFacts, mutexGroupFacts, negateFact, ptFacts, stateFacts)
import SAT

initialToSAT :: PlanningTask c -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ map (factHolds v 0) $ stateFacts $ ptInitalState pt

goalToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (factHolds v k) $ goalFacts $ ptGoal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add
  where
    pre = map (\f -> actionHolds v t a E.==> factHolds v (t - 1) f) $ Set.toList $ actionPre a
    add = map (\f -> actionHolds v t a E.==> factHolds v t f) $ Set.toList $ actionPost a

frameAxiom :: PlanningTask c -> Fact -> Time -> Map Variable E.Bit -> E.Bit
frameAxiom pt fact t v = E.or $ [factHolds v (t - 1) fact, factHolds v t $ negateFact fact] ++ actions
  where
    actions = map (actionHolds v t) $ filter (\a -> fact `elem` actionPost a) $ ptActions pt

mutexesToSAT :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
mutexesToSAT pt t v = mapM_ mutexToSAT (ptMutexGroups pt)
  where
    mutexToSAT mg = atMostOne $ map (factHolds v t) $ mutexGroupFacts mg

-- TODO: We don't need to do these if there are no constraints
actionCountToSAT :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
actionCountToSAT pt k v = do
  let actionBits t = map (actionHolds v t) $ ptActions pt
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
    extractAction t = case filter (actionHolds v t) $ ptActions pt of
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