module LTLConstraints (LTLFormula (..), pddlToLTL) where

import Basic (Fact, factToAtom, negateFact, showNamedList)
import Constraints (IsConstraints (..), Time, Variable, atLeastOneAction, exactlyOneAction, noAction, value)
import Control.Exception (assert)
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Ersatz as E
import GHC.Utils.Misc (nTimes)
import PDDLConstraints (PDDLFormula (..))

-- LTL constraints with an additional constraint Finally indicating that a fact should be true in the goal.
-- This is equivalent to LTL since Finally f is equivalent to "Globally (Not Next Top ==> f)".
data LTLFormula
  = Prop Fact
  | Not LTLFormula
  | And [LTLFormula]
  | Or [LTLFormula]
  | Eventually LTLFormula
  | Globally LTLFormula
  | Until LTLFormula LTLFormula
  | Next LTLFormula
  | WeakNext LTLFormula
  | Finally LTLFormula

instance Show LTLFormula where
  show (Prop fact) = show fact
  show (Not c) = "Not " ++ show c
  show (And []) = "Top"
  show (And cs) = showNamedList "And" cs
  show (Or []) = "Bot"
  show (Or cs) = showNamedList "Or" cs
  show (Eventually c) = "Eventually " ++ show c
  show (Globally c) = "Globally " ++ show c
  show (Until c1 c2) = showNamedList "Until" [c1, c2]
  show (Next c) = "Next " ++ show c
  show (WeakNext c) = "WeakNext " ++ show c
  show (Finally c) = "Finally " ++ show c

-- Helper for recursive transformations
recurse :: (LTLFormula -> LTLFormula) -> LTLFormula -> LTLFormula
recurse _ (Prop fact) = Prop fact
recurse f (Not c) = Not (f c)
recurse f (And cs) = And (map f cs)
recurse f (Or cs) = Or (map f cs)
recurse f (Eventually c) = Eventually (f c)
recurse f (Globally c) = Globally (f c)
recurse f (Until c1 c2) = Until (f c1) (f c2)
recurse f (Next c) = Next (f c)
recurse f (WeakNext c) = WeakNext (f c)
recurse f (Finally c) = Finally (f c)

-- Convert constraints to negation normal form
nnf :: LTLFormula -> LTLFormula
nnf (Not (Prop f)) = Prop $ negateFact f
nnf (Not (Not c)) = nnf c
nnf (Not (And cs)) = Or $ map (nnf . Not) cs
nnf (Not (Or cs)) = And $ map (nnf . Not) cs
nnf (Not (Eventually c)) = Globally $ nnf $ Not c
nnf (Not (Globally c)) = Eventually $ nnf $ Not c
nnf (Not (Until c1 c2)) =
  -- TODO: possibly exponential, add Release operator?
  Or [Globally $ nnf $ Not c2, Until (nnf $ Not c2) (And [nnf $ Not c1, nnf $ Not c2])]
nnf (Not (Next c)) = WeakNext $ nnf $ Not c
nnf (Not (WeakNext c)) = Next $ nnf $ Not c
nnf (Not (Finally c)) = Finally $ nnf $ Not c
nnf c = recurse nnf c

toSAT :: LTLFormula -> Time -> Time -> Map Variable E.Bit -> E.Bit
toSAT c0 t k v = assert (t <= k) $ case c0 of
  Prop fact -> value v t fact
  And cs -> E.and $ map (\c -> toSAT c t k v) cs
  Or cs -> E.or $ map (\c -> toSAT c t k v) cs
  Eventually c -> E.or [toSAT c t' k v | t' <- [t .. k]]
  Globally c -> E.and [toSAT c t' k v | t' <- [t .. k]]
  Until c1 c2 ->
    if t == k
      then toSAT c2 t k v
      else toSAT c2 t k v E.|| (toSAT c1 t k v E.&& toSAT (Until c1 c2) (t + 1) k v)
  Next c ->
    if t < k
      then E.and [toSAT c (t + 1) k v, exactlyOneAction v t, atLeastOneAction v (t + 1)]
      else E.false
  WeakNext c ->
    if t < k
      then exactlyOneAction v t E.&& ((toSAT c (t + 1) k v E.&& atLeastOneAction v (t + 1)) E.|| noAction v t)
      else E.true
  Finally c -> toSAT c k k v
  Not _ -> error "There should not be any negation in LTL-constraints when translating to SAT."

over :: Int -> LTLFormula -> LTLFormula
over n = nTimes n Next

(==>) :: LTLFormula -> LTLFormula -> LTLFormula
(==>) p q = Or [Not p, q]

weakUntil :: LTLFormula -> LTLFormula -> LTLFormula
weakUntil c1 c2 = Or [Until c1 c2, Globally c1]

instance IsConstraints LTLFormula where
  constraintsToSAT c k v = E.assert $ toSAT (nnf c) 0 k v

  constraintsAtoms (Prop fact) = Set.singleton $ factToAtom fact
  constraintsAtoms (Not c) = constraintsAtoms c
  constraintsAtoms (And cs) = Set.unions $ map constraintsAtoms cs
  constraintsAtoms (Or cs) = Set.unions $ map constraintsAtoms cs
  constraintsAtoms (Eventually c) = constraintsAtoms c
  constraintsAtoms (Globally c) = constraintsAtoms c
  constraintsAtoms (Until c1 c2) = Set.union (constraintsAtoms c1) (constraintsAtoms c2)
  constraintsAtoms (Next c) = constraintsAtoms c
  constraintsAtoms (WeakNext c) = constraintsAtoms c
  constraintsAtoms (Finally c) = constraintsAtoms c

pddlToLTL :: PDDLFormula -> LTLFormula
pddlToLTL (AtEnd p) = Finally $ Prop p
pddlToLTL (Always p) = Globally $ Prop p
pddlToLTL (Sometime p) = Eventually $ Prop p
pddlToLTL (Within t p) = Or $ map (\i -> over i $ Prop p) [0 .. t]
pddlToLTL (AtMostOnce p) =
  Globally $ Prop p ==> weakUntil (Prop p) (Globally $ Not $ Prop p)
pddlToLTL (SometimeBefore p q) =
  weakUntil
    (And [Not $ Prop p, Not $ Prop q])
    (And [Not $ Prop p, Prop q])
pddlToLTL (SometimeAfter p q) =
  Globally $ Prop p ==> Eventually (Prop q)
pddlToLTL (AlwaysWithin t p q) = Globally $ Prop p ==> pddlToLTL (Within t q)
pddlToLTL (HoldDuring t1 t2 p) = over t1 $ pddlToLTL (Within (t2 - 1 - t1) p)
pddlToLTL (HoldAfter t p) = over t $ Globally $ Prop p