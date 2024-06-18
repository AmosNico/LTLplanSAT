module LTL (Formula (..)) where
import Constraints ()
import Data.ByteString (ByteString)
import GHC.Utils.Misc (nTimes)

type Fact = Int
data Formula
  = Atom Fact
  | Not Formula
  | And [Formula]
  | Or [Formula]
  | E Formula -- Eventually
  | G Formula -- Globally
  | U Formula Formula -- Until
  | X Formula -- Next
  | X' Formula -- Weak Next
  | Finally Fact -- In the goal state


over :: Int -> Formula -> Formula
over n = nTimes n X

(-->) :: Formula -> Formula -> Formula
(-->) p q = Or [Not p, q]

{-constraintToLTL :: Constraint -> (ByteString -> Fact) -> Formula
constraintToLTL (AtEnd p) toFact =  Finally $ toFact p
constraintToLTL (Always p) toFact = G $ Atom $ toFact p
constraintToLTL (Sometime p) toFact = E $ Atom $ toFact p
constraintToLTL (Within t p) toFact = Or $ map (\i -> over i $ Atom $ toFact p) [0..t]
constraintToLTL (AtMostOnce p) toFact = G $ (Atom $ toFact p) --> Or []
constraintToLTL (SometimeBefore p q) toFact = F $ Atom $ toFact p
constraintToLTL (SometimeAfter p q) toFact = F $ Atom $ toFact p
constraintToLTL (AlwaysWithin t p q) toFact = F $ Atom $ toFact p
constraintToLTL (HoldDuring t dur p) toFact = F $ Atom $ toFact p
constraintToLTL (HoldAfter t p) toFact = F $ Atom $ toFact p-}
