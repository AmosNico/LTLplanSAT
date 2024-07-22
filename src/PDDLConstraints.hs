module PDDLConstraints (PDDLFormula (..), PDDLConstraints) where

import Basic (Fact, factToAtom)
import Constraints
import Control.Exception (assert)
import Data.Functor (void)
import qualified Data.Set as Set
import qualified Ersatz as E

-- These are the contraints of pddl 3 as introduced in "Plan Constraints and Preferences in PDDL3".
-- The paper is not very on what the constraint HoldAfter exactly means, so fro HoldAfter I follow the planning wiki.
data PDDLFormula
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

type PDDLConstraints = Constraints PDDLFormula

instance IsConstraints PDDLFormula where
  minimalTimeLimit (HoldDuring _ t2 _) = t2 - 1
  minimalTimeLimit (HoldAfter t _) = t
  minimalTimeLimit _ = 1

  constraintsToSAT (AtEnd f) k v = E.assert $ value v k f
  constraintsToSAT (Always f) k v = E.assert $ alwaysBetween 0 k f v
  constraintsToSAT (Sometime f) k v = E.assert $ sometimeBetween 0 k f v
  constraintsToSAT (Within n f) k v = do
    -- TODO: it is possible to allow for more parallellism once f has been true
    E.assert $ sometimeBetween 0 (min n k) f v
    E.assert $ E.and $ map (atMostOneAction v) [1 .. n]
  constraintsToSAT (AtMostOnce f) k v =
    void $ atMostOne $ map becomeTrueAt [1 .. k]
    where
      becomeTrueAt t = E.not (value v (t - 1) f) E.&& value v t f
  constraintsToSAT (SometimeAfter f1 f2) k v =
    E.assert $ E.and [value v t1 f1 E.==> after t1 | t1 <- [0 .. k]]
    where
      after t1 = sometimeBetween (t1 + 1) k f2 v
  constraintsToSAT (SometimeBefore f1 f2) k v =
    E.assert $ E.and [value v t1 f1 E.==> before t1 | t1 <- [0 .. k]]
    where
      before t1 = sometimeBetween 0 (t1 - 1) f2 v
  constraintsToSAT (AlwaysWithin n f1 f2) k v =
    E.assert $ E.and [value v t1 f1 E.==> within t1 | t1 <- [0 .. k]]
    where
      within t1 = sometimeBetween t1 (t1 + n) f2 v
  constraintsToSAT (HoldDuring n1 n2 f) k v = assert (n2 - 1 <= k) $ do
    E.assert $ alwaysBetween n1 (n2 - 1) f v
    E.assert $ E.and $ map (exactlyOneAction v) [1 .. n2 - 1]
  constraintsToSAT (HoldAfter n f) k v = assert (n <= k) $ do
    E.assert $ alwaysBetween n k f v
    E.assert $ E.and $ map (exactlyOneAction v) [1 .. n]

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