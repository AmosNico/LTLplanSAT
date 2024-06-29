module PDDLConstraints
  ( PDDLConstraint (..),
    PDDLConstraints (PDDLConstraints),
    singleHard,
    singleSoft,
    selectSoftConstraints,
  )
where

import Basic (Fact, factToAtom, showNamedList)
import Constraints (Constraints (..), alwaysBetween, atMostOne, sometimeBetween, value)
import Control.Monad (filterM)
import Data.Functor (void)
import Data.List ((\\))
import qualified Data.Set as Set
import qualified Ersatz as E
import System.Random (randomRIO)

-- These are the contraints of pddl 3 as introduced in "Plan Constraints and Preferences in PDDL3".
-- The paper is not very on what the constraint HoldAfter exactly means, so fro HoldAfter I follow the planning wiki.
data PDDLConstraint
  = AtEnd Fact
  | Always Fact
  | Sometime Fact
  | Within Int Fact -- Within t f means that f should be true at some time in the interval [0, t]
  | AtMostOnce Fact -- AtMostOnce f means that f can BECOME true at most once, i.e. it can only be true in one time interval.
  | SometimeAfter Fact Fact -- SometimeAfter f1 f2 means that if f1 is true, then at some time (strictly) after f2 should be true.
  | SometimeBefore Fact Fact -- SometimeBofore f1 f2 means that if f1 is true, then at some time (strictly) before f2 should be true.
  | AlwaysWithin Int Fact Fact -- combination of Always and Within
  | HoldDuring Int Int Fact -- HoldDuring t1 t2 f means that f should always be true in the interval [t1,t2).
  | HoldAfter Int Fact -- HoldAfter t f means that f should always be true after t (including at time t).
  deriving (Eq, Show)

instance Constraints PDDLConstraint where
  constraintsToSAT (AtEnd f) k v = E.assert $ value v k f
  constraintsToSAT (Always f) k v = E.assert $ alwaysBetween 0 k f v
  constraintsToSAT (Sometime f) k v = E.assert $ sometimeBetween 0 k f v
  constraintsToSAT (Within n f) k v = E.assert $ sometimeBetween 0 (min n k) f v
  constraintsToSAT (AtMostOnce f) k v =
    -- f should become true at most once
    void $ atMostOne [E.not (value v (t - 1) f) E.&& value v t f | t <- [1 .. k]]
  constraintsToSAT (SometimeAfter f1 f2) k v = E.assert $ E.and [value v t1 f1 E.==> after t1 | t1 <- [0 .. k]]
    where
      after t1 = sometimeBetween (t1 + 1) k f2 v
  constraintsToSAT (SometimeBefore f1 f2) k v = E.assert $ E.and [value v t1 f1 E.==> before t1 | t1 <- [0 .. k]]
    where
      before t1 = sometimeBetween 0 (t1 - 1) f2 v
  constraintsToSAT (AlwaysWithin n f1 f2) k v = E.assert $ E.and [value v t1 f1 E.==> within t1 | t1 <- [0 .. k]]
    where
      within t1 = sometimeBetween t1 (t1 + n) f2 v
  constraintsToSAT (HoldDuring n1 n2 f) k v = E.assert $ if k < n2 then E.false else alwaysBetween n1 (n2 - 1) f v
  constraintsToSAT (HoldAfter n f) k v = E.assert $ if k < n then E.false else alwaysBetween n k f v

  constraintsAtoms (AtEnd f) = Set.singleton $ factToAtom f
  constraintsAtoms (Always f) = Set.singleton $ factToAtom f
  constraintsAtoms (Sometime f) = Set.singleton $ factToAtom f
  constraintsAtoms (Within _ f) = Set.singleton $ factToAtom f
  constraintsAtoms (AtMostOnce f) = Set.singleton $ factToAtom f
  constraintsAtoms (SometimeAfter f1 f2) = Set.fromList $ map factToAtom [f1, f2]
  constraintsAtoms (SometimeBefore f1 f2) = Set.fromList $ map factToAtom [f1, f2]
  constraintsAtoms (AlwaysWithin _ f1 f2) = Set.fromList $ map factToAtom [f1, f2]
  constraintsAtoms (HoldDuring _ _ f) = Set.singleton $ factToAtom f
  constraintsAtoms (HoldAfter _ f) = Set.singleton $ factToAtom f

data PDDLConstraints
  = PDDLConstraints
      [PDDLConstraint] -- Hard constraints
      [PDDLConstraint] -- Selected soft constraints
      [PDDLConstraint] -- Ignored soft constraints

instance Show PDDLConstraints where
  show (PDDLConstraints hc sc ic) =
    showNamedList "Hard constraints:" hc
      ++ showNamedList "Selected soft constraints:" sc
      ++ showNamedList "Ignored soft constraints:" ic

instance Constraints PDDLConstraints where
  constraintsToSAT (PDDLConstraints hc sc _) t v = mapM_ (\c -> constraintsToSAT c t v) (hc ++ sc)

  constraintsAtoms (PDDLConstraints hc sc _) = Set.unions $ map constraintsAtoms $ hc ++ sc

instance Semigroup PDDLConstraints where
  PDDLConstraints hc1 sc1 ic1 <> PDDLConstraints hc2 sc2 ic2 =
    PDDLConstraints (hc1 <> hc2) (sc1 <> sc2) (ic1 <> ic2)

instance Monoid PDDLConstraints where
  mempty = PDDLConstraints mempty mempty mempty

singleHard, singleSoft :: PDDLConstraint -> PDDLConstraints
singleHard c = PDDLConstraints [c] [] []
singleSoft c = PDDLConstraints [] [] [c]

selectSoftConstraints :: PDDLConstraints -> Double -> IO PDDLConstraints
selectSoftConstraints (PDDLConstraints hc sc ic) probability = do
  let choose = (< probability) <$> randomRIO (0.0, 1.0)
  sc' <- filterM (const choose) (sc ++ ic)
  let ic' = (sc ++ ic) \\ sc'
  return (PDDLConstraints hc sc' ic')