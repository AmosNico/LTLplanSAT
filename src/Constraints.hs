{-# LANGUAGE DeriveFunctor #-}

module Constraints
  ( Time,
    Variable (..),
    Constraint (..),
    Constraints (..),
    IsConstraints (..),
    NoConstraint (..),
    singleHard,
    singleSoft,
    selectSoftConstraints,
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

import Basic (Action, Atom, Fact (..), showActionName, showNamedList)
import Control.Monad (filterM, replicateM)
import Data.ByteString (ByteString)
import Data.List ((\\))
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ersatz as E
import System.Random (randomRIO)

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

-- Given a formula type a, define the type of constraint over a
data Constraint a = Constraint
  { constraintID :: ByteString,
    constraintFormula :: a
  }
  deriving (Functor, Eq)

instance (Show a) => Show (Constraint a) where
  show = show . constraintFormula

data Constraints a
  = Constraints
      [Constraint a] -- Hard constraints
      [Constraint a] -- Selected soft constraints
      [Constraint a] -- Ignored soft constraints
  deriving (Functor)

instance (Show a) => Show (Constraints a) where
  show (Constraints hc sc ic) =
    showNamedList "Hard constraints:" hc
      ++ showNamedList "Selected soft constraints:" sc
      ++ showNamedList "Ignored soft constraints:" ic

instance Semigroup (Constraints a) where
  Constraints hc1 sc1 ic1 <> Constraints hc2 sc2 ic2 =
    Constraints (hc1 <> hc2) (sc1 <> sc2) (ic1 <> ic2)

instance Monoid (Constraints a) where
  mempty = Constraints mempty mempty mempty

-- This class captures the functionallity of constraints.
-- Each type of formula should be an instance of this class, and then
-- the instance of the corresponding Constraints type follows from below.
class (Show a) => IsConstraints a where
  -- Some constraints like HoldDuring need a minimal number of time steps.
  minimalTimeLimit :: a -> Time
  minimalTimeLimit _ = 1

  constraintsToSAT :: (E.MonadSAT s m) => a -> Time -> Map Variable E.Bit -> m ()

  constraintsAtoms :: a -> Set Atom

data NoConstraint = NoConstraint

instance Show NoConstraint where
  show NoConstraint = "No constraints"

instance IsConstraints NoConstraint where
  constraintsToSAT _ _ _ = return ()

  constraintsAtoms _ = Set.empty

instance (IsConstraints a) => IsConstraints (Constraints a) where
  minimalTimeLimit (Constraints hc sc _) = maximum $ 1 : map (minimalTimeLimit . constraintFormula) (hc ++ sc)

  constraintsToSAT (Constraints hc sc _) t v = mapM_ (\c -> constraintsToSAT (constraintFormula c) t v) (hc ++ sc)

  constraintsAtoms (Constraints hc sc _) = Set.unions $ map (constraintsAtoms . constraintFormula) $ hc ++ sc

singleHard, singleSoft :: Constraint a -> Constraints a
singleHard c = Constraints [c] [] []
singleSoft c = Constraints [] [] [c]

selectSoftConstraints :: (Eq a) => Constraints a -> Double -> IO (Constraints a)
selectSoftConstraints (Constraints hc sc ic) probability = do
  let choose = (< probability) <$> randomRIO (0.0, 1.0)
  sc' <- filterM (const choose) (sc ++ ic)
  let ic' = (sc ++ ic) \\ sc'
  return (Constraints hc sc' ic')

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