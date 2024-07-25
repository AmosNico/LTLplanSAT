{-# LANGUAGE TupleSections #-}

module SAT
  ( Time,
    Plan (..),
    Variable (..),
    defineVariables,
    factHolds,
    actionHolds,
    atMostOneAction,
    atLeastOneAction,
    exactlyOneAction,
    noAction,
    sometimeBetween,
    alwaysBetween,
    conditionalAtMostOne,
    atMostOne,
  )
where

import Control.Monad (replicateM)
import Data.List (intercalate)
import Data.Map (Map, fromList, (!))
import qualified Ersatz as E
import PlanningTask (Action, Atom, Fact (..), PlanningTask, actionCost, ptActions, ptAtoms, showActionName)

-- Time steps in the SAT-solver. This is not the same as the number of actions in the sat solver.
type Time = Int

newtype Plan = Plan [Action]

instance Show Plan where
  show (Plan as) =
    "Plan with length "
      ++ show (length as)
      ++ " and cost "
      ++ show (sum $ map actionCost as)
      ++ ":\n  "
      ++ intercalate "\n  " (map showActionName as)

-- These are the most important variables handed to the SAT-solver.
-- Ersatz itself introduces additional variables for most formulas,
-- and locally other variables can be introduced as well, but all variables which can be
-- accessed in different parts of the code should be listed here and declared in `defineVariables`.
data Variable
  = ActionVar Time Action -- ActionVar t action indicates that action is performed at t
  | AtomVar Time Atom -- AtomVar t atom indicates that PosAtom atom is true at t
  | AtMostOneAction Time -- AtMostOneAction t indicates whether at most one action can (not is) preformed
  | AtLeastOneAction Time -- AtLeastOneAction t indicates whether at least one action has to (not is) preformed
  | NoAction Time -- NoAction t indicates that no action can happen at time t
  deriving (Eq, Ord)

instance Show Variable where
  show (ActionVar t action) = "ActionVariable " ++ show t ++ " " ++ showActionName action
  show (AtomVar t atom) = "AtomVariable " ++ show t ++ " " ++ show atom
  show (AtMostOneAction t) = "AtMostOneAction " ++ show t
  show (AtLeastOneAction t) = "AtLeastOneAction " ++ show t
  show (NoAction t) = "NoAction " ++ show t

defineVariables :: (E.MonadSAT s m) => PlanningTask c -> Time -> m (Map Variable E.Bit)
defineVariables pt k = sequence $ fromList $ map (,E.exists) vars
  where
    vars = actionVars ++ atomVars ++ amoVars ++ aloVars ++ noaVars
    actionVars = [ActionVar t a | a <- ptActions pt, t <- [1 .. k]]
    atomVars = [AtomVar t atom | atom <- ptAtoms pt, t <- [0 .. k]]
    amoVars = [AtMostOneAction t | t <- [1 .. k]]
    aloVars = [AtLeastOneAction t | t <- [1 .. k]]
    noaVars = [NoAction t | t <- [1 .. k]]

factHolds :: Map Variable E.Bit -> Time -> Fact -> E.Bit
factHolds v t (PosAtom atom) = v ! AtomVar t atom
factHolds v t (NegAtom atom) = E.not $ v ! AtomVar t atom

-- b can be either Bool or E.Bit
actionHolds :: Map Variable b -> Time -> Action -> b
actionHolds v t action = v ! ActionVar t action

atMostOneAction, atLeastOneAction, exactlyOneAction, noAction :: Map Variable E.Bit -> Time -> E.Bit
atMostOneAction v t = v ! AtMostOneAction t
atLeastOneAction v t = v ! AtLeastOneAction t
exactlyOneAction v t = atMostOneAction v t E.&& atLeastOneAction v t
noAction v t = v ! NoAction t

sometimeBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
sometimeBetween t1 t2 f v = E.or [factHolds v t f | t <- [t1 .. t2]]

alwaysBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
alwaysBetween t1 t2 f v = E.and [factHolds v t f | t <- [t1 .. t2]]

-- TODO: quadratic encoding for smaller of (length l)
conditionalAtMostOne :: (E.MonadSAT s m) => E.Bit -> [E.Bit] -> m E.Bits
conditionalAtMostOne condition l = do
  let n = ceiling (logBase 2 $ fromIntegral $ length l :: Double)
  bits <- E.Bits <$> replicateM n E.exists
  let constr idx option = option E.==> (bits E.=== E.encode idx)
  E.assert $ condition E.==> E.and (zipWith constr [0 ..] l)
  return bits

atMostOne :: (E.MonadSAT s m) => [E.Bit] -> m E.Bits
atMostOne = conditionalAtMostOne E.true