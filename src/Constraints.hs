module Constraints
  ( NoConstraint (..),
    Constraints (..),
    value,
    sometimeBetween,
    alwaysBetween,
    atMostOne,
  )
where

import Control.Monad (replicateM)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map, (!))
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Ersatz as E
import Types (Atom, Fact (..), Time, Variable (AtomV))

-- This class captures the functionallity that is requires for constraints.
-- The instance of the functor class ensures that the facts of constraints (which are typically read as bytestrings),
-- can be transformed into the integers that PlanningTask uses.
class Constraints c where
  constraintsToSAT :: (E.MonadSAT s m) => c -> Time -> Map Variable E.Bit -> m ()

  constraintsAtoms :: c -> Set Atom

  showConstraints :: c -> ByteString

data NoConstraint = NoConstraint

instance Constraints NoConstraint where
  constraintsToSAT _ _ _ = return ()

  constraintsAtoms _ = Set.empty

  showConstraints NoConstraint = C8.pack "No constraints"

value :: Map Variable E.Bit -> Time -> Fact -> E.Bit
value v t (PosAtom atom) = v ! AtomV t atom
value v t (NegAtom atom) = E.not $ v ! AtomV t atom

sometimeBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
sometimeBetween t1 t2 f v = E.or [value v t f | t <- [t1 .. t2]]

alwaysBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
alwaysBetween t1 t2 f v = E.and [value v t f | t <- [t1 .. t2]]

atMostOne :: (E.MonadSAT s m) => [E.Bit] -> m E.Bits
atMostOne l = do
  let n = ceiling (logBase 2 $ fromIntegral $ length l :: Double)
  bits <- E.Bits <$> replicateM n E.exists
  let constr idx option = option E.==> (bits E.=== E.encode idx)
  E.assert $ E.and $ zipWith constr [0 ..] l
  return bits