module LTLConstraints (LTLConstraint (..), pddlToLTL) where

import Constraints (Constraints (..), value)
import Control.Exception (assert)
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map)
import qualified Data.Set as Set
import qualified Ersatz as E
import GHC.Utils.Misc (nTimes)
import PDDLConstraints (PDDLConstraint (..))
import Basic

data LTLConstraint
  = Prop Fact
  | Not LTLConstraint
  | And [LTLConstraint]
  | Or [LTLConstraint]
  | Eventually LTLConstraint -- E
  | Globally LTLConstraint -- G
  | Until LTLConstraint LTLConstraint -- U
  | Next LTLConstraint -- X
  | WeakNext LTLConstraint -- x'
  | Finally LTLConstraint -- In the goal state
  deriving (Show)

toSAT :: LTLConstraint -> Time -> Time -> Map Variable E.Bit -> E.Bit
toSAT c0 t k v = assert (t <= k) $ case c0 of
  (Prop fact) -> value v t fact
  (Not c) -> E.not $ toSAT c t k v
  (And cs) -> E.and $ map (\c -> toSAT c t k v) cs
  (Or cs) -> E.or $ map (\c -> toSAT c t k v) cs
  (Eventually c) -> E.or [toSAT c t' k v | t' <- [t .. k]]
  (Globally c) -> E.and [toSAT c t' k v | t' <- [t .. k]]
  (Until c1 c2) ->
    if t == k
      then toSAT c2 t k v
      else toSAT c2 t k v E.|| (toSAT c1 t k v E.&& toSAT (Until c1 c2) (t + 1) k v)
  (Next c) -> if t < k then toSAT c (t + 1) k v else E.false
  (WeakNext c) -> if t < k then toSAT c (t + 1) k v else E.true
  (Finally c) -> toSAT c k k v

over :: Int -> LTLConstraint -> LTLConstraint
over n = nTimes n Next

(==>) :: LTLConstraint -> LTLConstraint -> LTLConstraint
(==>) p q = Or [Not p, q]

weakUntil :: LTLConstraint -> LTLConstraint -> LTLConstraint
weakUntil c1 c2 = Or [Until c1 c2, Globally c1]

instance Constraints LTLConstraint where
  constraintsToSAT c k v = E.assert $ toSAT c 0 k v

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

  showConstraints c = C8.pack $ show c

pddlToLTL :: PDDLConstraint -> LTLConstraint
pddlToLTL (AtEnd p) = Finally $ Prop p
pddlToLTL (Always p) = Globally $ Prop p
pddlToLTL (Sometime p) = Eventually $ Prop p
pddlToLTL (Within t p) = Or $ map (\i -> over i $ Prop p) [0 .. t]
pddlToLTL (AtMostOnce p) =
  Globally $ Prop p ==> weakUntil (Prop p) (Not $ Prop p)
pddlToLTL (SometimeBefore p q) =
  weakUntil
    (And [Not $ Prop p, Not $ Prop q])
    (And [Not $ Prop p, Prop q])
pddlToLTL (SometimeAfter p q) =
  Globally $ Prop p ==> Eventually (Prop q)
pddlToLTL (AlwaysWithin t p q) = Globally $ Prop p ==> pddlToLTL (Within t q)
pddlToLTL (HoldDuring t1 t2 p) = over t1 $ pddlToLTL (Within (t2 - 1 - t1) p)
pddlToLTL (HoldAfter t p) = over t $ Globally $ Prop p
