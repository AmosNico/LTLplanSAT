{-# LANGUAGE TupleSections #-}

module ExistsStepSAT (existsStepEncoding, extractExistsStepPlan) where

import Basic (Action (..), negateFact)
import Constraints (IsConstraints, Time, Variable (ActionVar))
import Control.Monad.State (StateT)
import qualified Data.Graph as Graph
import Data.List (sortOn)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Tree (flatten)
import Data.Tuple.Extra (snd3)
import qualified Ersatz as E
import PlanningTask (PlanningTask (ptActions), ptFacts)
import SequentialSAT (Plan (..), basicSATEncoding)

-- Adjacency list for the disabling graph.
-- We don't use nodes (first element), just keys (second element).
disablingGraphEdges :: PlanningTask c -> [((), Action, [Action])]
disablingGraphEdges pt = map (\a -> ((), a, disables a)) $ ptActions pt
  where
    requireNeg a' = Set.map negateFact $ actionPre a'
    conflicting a a' = not $ null $ Set.intersection (actionPost a) (requireNeg a')
    consistentPre a a' = null $ Set.intersection (actionPre a) (requireNeg a')
    disables a = filter (\a' -> conflicting a a' && consistentPre a a') $ ptActions pt

-- Strongly connected components in the disabling graph in reverse topological order
disablingGraphSCC :: PlanningTask c -> [[Action]]
disablingGraphSCC pt = map (map $ snd3 . atomFromVertex) components
  where
    (graph, atomFromVertex, _) = Graph.graphFromEdges $ disablingGraphEdges pt
    components = map flatten $ Graph.scc graph

orderActions :: PlanningTask c -> Action -> Int
orderActions pt a = order ! a
  where
    order = Map.fromList $ zip (concat $ disablingGraphSCC pt) [1 ..]

-- Find all pairs (fst,snd) in the list such that isFst fst and isSnd snd such that
-- fst appears before snd in l and there are no elements between fst and snd which satisfy isSnd.
-- To find these pairs we go through the list in reverse order and remember the previous element
-- that satisfies isSnd
consecutivePairs :: [Variable] -> (Variable -> Bool) -> (Variable -> Bool) -> [(Variable, Variable)]
consecutivePairs l isFst isSnd = f (reverse l) Nothing
  where
    f [] _ = []
    f (x : xs) Nothing = if isSnd x then f xs (Just x) else f xs Nothing
    f (x : xs) (Just y) = if isFst x then (x, y) : recurse else recurse
      where
        recurse = if isSnd x then f xs (Just x) else f xs (Just y)

-- The first argument is an ordering of variables, the second argument indicates which varibable
-- erase a certain property, the third argument indicates which variables require that the property
-- has not been erased by previous variables. Given an interpretation of the variables (forth argument)
-- the function states that this constraint should be true within the SAT monad m.
chain :: (E.MonadSAT s m) => [Variable] -> (Variable -> Bool) -> (Variable -> Bool) -> Map Variable E.Bit -> m ()
chain l isErasing isRequiring v = do
  let req = filter isRequiring l
  -- forbid ! var indicates that var is forbidden.
  forbid <- sequence $ Map.fromList $ map (,E.exists) req
  -- a variable cannot be forbidden and true
  let c1 = [E.nand [forbid ! var, v ! var] | var <- req]
  -- if a requiring variable is forbidden, then all following requiring variables are forbidden
  let c2 = [forbid ! var1 E.==> forbid ! var2 | (var1, var2) <- consecutivePairs l isRequiring isRequiring]
  -- if a erasing variable is true, then the next requiring variable is forbidden
  let c3 = [v ! var1 E.==> forbid ! var2 | (var1, var2) <- consecutivePairs l isErasing isRequiring]
  E.assert $ E.and $ c1 ++ c2 ++ c3

variableToAction :: Variable -> Action
variableToAction (ActionVar _ action) = action
variableToAction v = error $ "Expected an action variable, but got " ++ show v

existsStep :: (E.MonadSAT s m) => PlanningTask c -> Time -> Map Variable E.Bit -> m ()
existsStep pt k v = sequence_ [constraint t fact scc | scc <- disablingGraphSCC pt, fact <- ptFacts pt, t <- [1 .. k]]
  where
    variables t = map (ActionVar t)
    isErasing fact variable = negateFact fact `elem` actionPost (variableToAction variable)
    isRequiring fact variable = fact `elem` actionPre (variableToAction variable)
    -- for each timestep, fact and scc, require that scc does not contain
    -- both an action that requires the fact and one that erases it.
    constraint t fact scc = do
      chain (variables t scc) (isErasing fact) (isRequiring fact) v
      chain (variables t $ reverse scc) (isErasing fact) (isRequiring fact) v

-- TODO: some constraints don't allow parallel actions constraints
existsStepEncoding :: (IsConstraints c) => PlanningTask c -> Time -> StateT E.SAT IO (Map Variable E.Bit)
existsStepEncoding pt k = do
  vars <- basicSATEncoding pt k
  existsStep pt k vars
  -- constraintsToSAT (ptConstraints pt) k vars
  return vars

extractExistsStepPlan :: PlanningTask c -> Time -> Map Variable Bool -> Plan
extractExistsStepPlan pt k v = Plan $ concatMap extractActions [1 .. k]
  where
    sortActions = sortOn (orderActions pt)
    extractActions t = sortActions $ filter (\a -> v ! ActionVar t a) $ ptActions pt