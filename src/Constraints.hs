{-# LANGUAGE DeriveFunctor #-}

module Constraints
  ( Constraint (..),
    Constraints (..),
    activeConstraints,
    IsConstraints (..),
    NoConstraint (..),
    singleHard,
    singleSoft,
    SelectSoftConstraints (..),
    selectSoftConstraints,
  )
where

import Control.Monad (filterM)
import Data.ByteString (ByteString)
import Data.List (partition, (\\))
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ersatz as E
import PlanningTask (Atom, showNamedList)
import SAT (Time, Variable)
import System.Random (randomRIO)

-- Given a formula type a, define the type of constraint over a
data Constraint a = Constraint
  { constraintId :: ByteString,
    constraintFormula :: a
  }
  deriving (Functor)

instance Eq (Constraint a) where
  (==) c1 c2 = constraintId c1 == constraintId c2

instance (Show a) => Show (Constraint a) where
  show = show . constraintFormula

data Constraints a
  = Constraints
      [Constraint a] -- Hard constraints
      [Constraint a] -- Selected soft constraints
      [Constraint a] -- Ignored soft constraints
  deriving (Functor)

activeConstraints :: Constraints a -> [Constraint a]
activeConstraints (Constraints hard soft _) = hard ++ soft

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

selectRandomSoftConstraints :: Double -> Constraints a -> IO (Constraints a)
selectRandomSoftConstraints probability (Constraints hc sc ic) = do
  let choose = (< probability) <$> randomRIO (0.0, 1.0)
  sc' <- filterM (const choose) (sc ++ ic)
  let ic' = (sc ++ ic) \\ sc'
  return (Constraints hc sc' ic')

-- TODO check that all ids are valid ids and notify user if this is not the case
selectGivenSoftConstraints :: Set ByteString -> Constraints a -> Constraints a
selectGivenSoftConstraints ids (Constraints hc sc ic) = Constraints hc sc' ic'
  where
    (sc', ic') = partition ((`elem` ids) . constraintId) (sc ++ ic)

data SelectSoftConstraints = SelectRandom Double | SelectGiven (Set ByteString)

selectSoftConstraints :: SelectSoftConstraints -> Constraints a -> IO (Constraints a)
selectSoftConstraints (SelectRandom probability) constraints = selectRandomSoftConstraints probability constraints
selectSoftConstraints (SelectGiven ids) constraints = return $ selectGivenSoftConstraints ids constraints