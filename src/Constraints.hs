module Constraints
  ( Time,
    Variable (..),
    Constraints (..),
    NoConstraint (..),
    value,
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

import Basic (Action, Atom, Fact (..), showActionName)
import Control.Monad (replicateM)
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ersatz as E

-- Time steps in the SAT-solver. This is not the same as the number of actions in the sat solver.
type Time = Int

-- Variables used in the SAT-solver
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

-- This class captures the functionallity that is requires for constraints.
class (Show c) => Constraints c where
  -- Some constraints like HoldDuring need a minimal number of time steps.
  minimalTimeLimit :: c -> Time
  minimalTimeLimit _ = 1

  constraintsToSAT :: (E.MonadSAT s m) => c -> Time -> Map Variable E.Bit -> m ()

  constraintsAtoms :: c -> Set Atom

data NoConstraint = NoConstraint

instance Show NoConstraint where
  show NoConstraint = "No constraints"

instance Constraints NoConstraint where
  constraintsToSAT _ _ _ = return ()

  constraintsAtoms _ = Set.empty

value :: Map Variable E.Bit -> Time -> Fact -> E.Bit
value v t (PosAtom atom) = v ! AtomVar t atom
value v t (NegAtom atom) = E.not $ v ! AtomVar t atom

atMostOneAction, atLeastOneAction, exactlyOneAction, noAction :: Map Variable E.Bit -> Time -> E.Bit
atMostOneAction v t = v ! AtMostOneAction t
atLeastOneAction v t = v ! AtLeastOneAction t
exactlyOneAction v t = atMostOneAction v t E.&& atLeastOneAction v t
noAction v t = v ! NoAction t

sometimeBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
sometimeBetween t1 t2 f v = E.or [value v t f | t <- [t1 .. t2]]

alwaysBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
alwaysBetween t1 t2 f v = E.and [value v t f | t <- [t1 .. t2]]

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