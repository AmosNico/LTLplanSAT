{-# LANGUAGE DeriveFunctor #-}

module PDDLConstraints
  ( PDDLConstraint (..),
    PDDLConstraints (..),
    singleHard,
    singleSoft,
  )
where

import Constraints (Constraints (..), alwaysBetween, sometimeBetween, atMostOne, defineAtMostOneVariables)
import qualified Data.ByteString.Char8 as C8
import Data.Map ((!))
import qualified Data.Map as Map
import qualified Ersatz as E
import Types (Variable (FactV, AtMostOnceV))

data PDDLConstraint a
  = AtEnd a
  | Always a
  | Sometime a
  | Within Int a
  | AtMostOnce a
  | SometimeAfter a a
  | SometimeBefore a a
  | AlwaysWithin Int a a
  | HoldDuring Int Int a
  | HoldAfter Int a
  deriving (Functor, Show)

data PDDLConstraints a = PDDLConstraints
  { hardConstraints :: [PDDLConstraint a],
    softConstraints :: [PDDLConstraint a]
  }
  deriving (Functor)

instance Semigroup (PDDLConstraints a) where
  PDDLConstraints hc1 sc1 <> PDDLConstraints hc2 sc2 =
    PDDLConstraints (hc1 <> hc2) (sc1 <> sc2)

instance Monoid (PDDLConstraints a) where
  mempty = PDDLConstraints mempty mempty

singleHard, singleSoft :: PDDLConstraint a -> PDDLConstraints a
singleHard c = PDDLConstraints [c] []
singleSoft c = PDDLConstraints [] [c]

instance Constraints PDDLConstraint where
  constraintsToSat (AtEnd f) k v = v ! FactV k f
  constraintsToSat (Always f) k v = alwaysBetween 0 k f v
  constraintsToSat (Sometime f) k v = sometimeBetween 0 k f v
  constraintsToSat (Within n f) k v = sometimeBetween 0 (min n k) f v
  constraintsToSat (AtMostOnce f) k v = atMostOne (AtMostOnceV f) [FactV t f | t <- [0..k]] v
  constraintsToSat (SometimeAfter f1 f2) k v = E.and [v ! FactV t1 f1 E.==> after t1 | t1 <- [0 .. k]]
    where
      after t1 = sometimeBetween (t1 + 1) k f2 v
  constraintsToSat (SometimeBefore f1 f2) k v = E.and [v ! FactV t1 f1 E.==> before t1 | t1 <- [0 .. k]]
    where
      before t1 = sometimeBetween 0 (t1 - 1) f2 v
  constraintsToSat (AlwaysWithin n f1 f2) k v = E.and [v ! FactV t1 f1 E.==> within t1 | t1 <- [0 .. k]]
    where
      within t1 = sometimeBetween t1 (t1 + n) f2 v
  constraintsToSat (HoldDuring n1 n2 f) k v = if k < n2 then E.false else alwaysBetween n1 n2 f v
  constraintsToSat (HoldAfter n f) k v = if k < n then E.false else alwaysBetween n k f v

  defineExtraVariables (AtMostOnce f) k = defineAtMostOneVariables (AtMostOnceV f) k
  defineExtraVariables _ _ = return Map.empty

  -- TOTO: this is not nice
  showConstraints showA c = C8.pack $ show $ fmap showA c

-- TODO: What with soft constraints?
instance Constraints PDDLConstraints where
  constraintsToSat (PDDLConstraints hc sc) t v = E.and $ map (\c -> constraintsToSat c t v) hc

  defineExtraVariables (PDDLConstraints hc sc) k = Map.unions <$> mapM (`defineExtraVariables` k) hc

  showConstraints showA (PDDLConstraints hc sc) =
    C8.concat
      [ C8.pack "Hard constraints:",
        C8.concat (map (C8.append (C8.pack "\n  ") . showConstraints showA) hc),
        C8.pack "\nSoftConstraints:",
        C8.concat (map (C8.append (C8.pack "\n  ") . showConstraints showA) sc),
        C8.pack "\n"
      ]