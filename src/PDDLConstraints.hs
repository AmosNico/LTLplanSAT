{-# LANGUAGE OverloadedStrings #-}

module PDDLConstraints
  ( PDDLConstraint (..),
    PDDLConstraints (PDDLConstraints),
    singleHard,
    singleSoft,
    selectSoftConstraints,
  )
where

import Constraints (Constraints (..), alwaysBetween, atMostOne, defineAtMostOneVariables, sometimeBetween, value)
import Control.Monad (filterM)
import qualified Data.ByteString.Char8 as C8
import Data.List ((\\))
import qualified Data.Map as Map
import qualified Ersatz as E
import System.Random (initStdGen, randomRIO)
import Types (Fact, Variable (..))

data PDDLConstraint
  = AtEnd Fact
  | Always Fact
  | Sometime Fact
  | Within Int Fact
  | AtMostOnce Fact
  | SometimeAfter Fact Fact
  | SometimeBefore Fact Fact
  | AlwaysWithin Int Fact Fact
  | HoldDuring Int Int Fact
  | HoldAfter Int Fact
  deriving (Eq, Show)

data PDDLConstraints
  = PDDLConstraints
      [PDDLConstraint] -- Hard constraints
      [PDDLConstraint] -- Selected soft constraints
      [PDDLConstraint] -- Ignored soft constraints

instance Semigroup PDDLConstraints where
  PDDLConstraints hc1 sc1 ic1 <> PDDLConstraints hc2 sc2 ic2 =
    PDDLConstraints (hc1 <> hc2) (sc1 <> sc2) (ic1 <> ic2)

instance Monoid PDDLConstraints where
  mempty = PDDLConstraints mempty mempty mempty

singleHard, singleSoft :: PDDLConstraint -> PDDLConstraints
singleHard c = PDDLConstraints [c] [] []
singleSoft c = PDDLConstraints [] [] [c]

selectSoftConstraints :: PDDLConstraints -> IO PDDLConstraints
selectSoftConstraints (PDDLConstraints hc sc ic) = do
  let choose = (< (0.3 :: Float)) <$> randomRIO (0.0, 1.0)
  sc' <- filterM (const choose) (sc ++ ic)
  let ic' = (sc ++ ic) \\ sc'
  return (PDDLConstraints hc sc' ic')

instance Constraints PDDLConstraint where
  constraintsToSat (AtEnd f) k v = value v k f
  constraintsToSat (Always f) k v = alwaysBetween 0 k f v
  constraintsToSat (Sometime f) k v = sometimeBetween 0 k f v
  constraintsToSat (Within n f) k v = sometimeBetween 0 (min n k) f v
  constraintsToSat (AtMostOnce f) k v = E.true -- atMostOne (AtMostOnceV f) [FactV t f | t <- [0 .. k]] v
  constraintsToSat (SometimeAfter f1 f2) k v = E.and [value v t1 f1 E.==> after t1 | t1 <- [0 .. k]]
    where
      after t1 = sometimeBetween (t1 + 1) k f2 v
  constraintsToSat (SometimeBefore f1 f2) k v = E.and [value v t1 f1 E.==> before t1 | t1 <- [0 .. k]]
    where
      before t1 = sometimeBetween 0 (t1 - 1) f2 v
  constraintsToSat (AlwaysWithin n f1 f2) k v = E.and [value v t1 f1 E.==> within t1 | t1 <- [0 .. k]]
    where
      within t1 = sometimeBetween t1 (t1 + n) f2 v
  constraintsToSat (HoldDuring n1 n2 f) k v = if k < n2 then E.false else alwaysBetween n1 n2 f v
  constraintsToSat (HoldAfter n f) k v = if k < n then E.false else alwaysBetween n k f v

  defineExtraVariables (AtMostOnce f) k = defineAtMostOneVariables (AtMostOnceV f) k
  defineExtraVariables _ _ = return Map.empty

  -- TODO: this is not nice
  showConstraints c = C8.pack $ show c

-- TODO: What with soft constraints?
instance Constraints PDDLConstraints where
  constraintsToSat (PDDLConstraints hc sc _) t v = E.and $ map (\c -> constraintsToSat c t v) (hc ++ sc)

  defineExtraVariables (PDDLConstraints hc sc _) k = Map.unions <$> mapM (`defineExtraVariables` k) (hc ++ sc)

  showConstraints (PDDLConstraints hc sc ic) =
    C8.concat
      [ "Hard constraints:",
        C8.concat (map (C8.append "\n  " . showConstraints) hc),
        "\nSelected soft constraints:",
        C8.concat (map (C8.append "\n  " . showConstraints) sc),
        "\nIgnored soft constraints:",
        C8.concat (map (C8.append "\n  " . showConstraints) ic)
      ]