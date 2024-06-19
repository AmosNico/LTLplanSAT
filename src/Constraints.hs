{-# LANGUAGE TupleSections #-}

module Constraints
  ( NoConstraint (..),
    Constraints (..),
    value,
    sometimeBetween,
    alwaysBetween,
    atMostOneVariables,
    defineAtMostOneVariables,
    atMostOne,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Ersatz as E
import Types (Fact (..), Time, Variable (AtomV))

-- This class captures the functionallity that is requires for constraints.
-- The instance of the functor class ensures that the facts of constraints (which are typically read as bytestrings),
-- can be transformed into the integers that PlanningTask uses.
class Constraints c where
  constraintsToSat :: c -> Time -> Map Variable E.Bit -> E.Bit

  defineExtraVariables :: (E.MonadSAT s m) => c -> Time -> m (Map Variable E.Bit)

  showConstraints :: c -> ByteString

data NoConstraint = NoConstraint

instance Constraints NoConstraint where
  constraintsToSat _ _ _ = E.true
  defineExtraVariables _ _ = return Map.empty
  showConstraints NoConstraint = C8.pack "No constraints"

value :: Map Variable E.Bit -> Time -> Fact -> E.Bit
value v t (PosAtom atom) = v ! AtomV t atom
value v t (NegAtom atom) = E.not $ v ! AtomV t atom

sometimeBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
sometimeBetween t1 t2 f v = E.or [value v t f | t <- [t1 .. t2]]

alwaysBetween :: Time -> Time -> Fact -> Map Variable E.Bit -> E.Bit
alwaysBetween t1 t2 f v = E.and [value v t f | t <- [t1 .. t2]]

-- For the following functions the first argument is expected to be either atMostOnceV fact or atMostOneActionV time.
atMostOneVariables :: (Int -> Variable) -> Int -> [Variable]
atMostOneVariables toAMOVariable nOptions = [toAMOVariable i | i <- [0 .. n - 1]]
  where
    n = ceiling (logBase 2 $ fromIntegral nOptions :: Double)

defineAtMostOneVariables :: (E.MonadSAT s m) => (Int -> Variable) -> Int -> m (Map Variable E.Bit)
defineAtMostOneVariables nOptions toAMOVariable =
  sequence $ Map.fromList $ map (,E.exists) $ atMostOneVariables nOptions toAMOVariable

atMostOne :: (Int -> Variable) -> [Variable] -> Map Variable E.Bit -> E.Bit
atMostOne toAmoVariable vars v = E.and $ zipWith constr [0 ..] vars
  where
    amoVars = atMostOneVariables toAmoVariable (length vars)
    bits = E.Bits $ map (v !) amoVars
    constr idx var = v ! var E.==> (bits E.=== E.encode idx)
