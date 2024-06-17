{-# LANGUAGE DeriveFunctor #-}

module Constraints
  ( NoConstraint (..),
    PDDLConstraint (..),
    PDDLConstraints (..),
    singleHard,
    singleSoft,
    Constraints (..),
  )
where

import Data.Map (Map)
import qualified Ersatz as E
import Types (Fact, Time, Variable)

data NoConstraint c = NoConstraint
  deriving (Functor)

data PDDLConstraint c
  = AtEnd c
  | Always c
  | Sometime c
  | Within Int c
  | AtMostOnce c
  | SometimeAfter c c
  | SometimeBefore c c
  | AlwaysWithin Int c c
  | HoldDuring Int Int c
  | HoldAfter Int c
  deriving (Show, Functor)

data PDDLConstraints c = PDDLConstraints
  { hardConstraints :: [PDDLConstraint c],
    softConstraints :: [PDDLConstraint c]
  }
  deriving (Functor)

instance Semigroup (PDDLConstraints c) where
  PDDLConstraints hc1 sc1 <> PDDLConstraints hc2 sc2 =
    PDDLConstraints (hc1 <> hc2) (sc1 <> sc2)

instance Monoid (PDDLConstraints c) where
  mempty = PDDLConstraints mempty mempty

singleHard, singleSoft :: PDDLConstraint a -> PDDLConstraints a
singleHard c = PDDLConstraints [c] []
singleSoft c = PDDLConstraints [] [c]

-- This class captures the functionallity that is requires for constraints.
-- The instance of the functor class ensures that the facts of constraints (which are typically read as bytestrings),
-- can be transformed into the integers that PlanningTask uses.
class (Functor c) => Constraints c where
  constraintsToSat :: c Fact -> Time -> Map Variable E.Bit -> E.Bit

instance Constraints NoConstraint where
  constraintsToSat _ _ _ = E.true

-- TODO: implement
instance Constraints PDDLConstraint

-- TODO: What with soft constraints?
instance Constraints PDDLConstraints where
  constraintsToSat (PDDLConstraints hc sc) t v = E.and $ map (\c -> constraintsToSat c t v) hc