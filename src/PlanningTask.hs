module PlanningTask (fromSAS) where

import Constraints (NoConstraint (..))
import Data.List (find)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import SAS (SAS)
import qualified SAS
import Types

-- Compute the hash of v = 0 for all SAS variables v. The last element is the number of facts
perfectHash :: SAS -> Vector Int
perfectHash sas = Vec.unfoldrExactN (SAS.nVars sas + 1) f (0, 0)
  where
    -- f iterates through all variables v and computes the hash for v = 0,
    -- while remembering number of the variable and hash value
    f (var, hash) = (hash, (var + 1, hash + SAS.domainSize sas var))

convertFact :: SAS -> SAS.Fact -> Fact
convertFact sas (var, val) = (perfectHash sas Vec.! var) + val

convertFacts :: SAS -> [SAS.Fact] -> [Fact]
convertFacts sas = map (convertFact sas)

inverseHash :: SAS -> Int -> Vector SAS.Fact
inverseHash sas nFacts = Vec.unfoldrExactN nFacts f (0, 0)
  where
    f (var, val) =
      if val + 1 == SAS.domainSize sas var
        then ((var, val), (var + 1, 0))
        else ((var, val), (var, val + 1))

deletingEffects :: SAS -> SAS.Action -> [Fact]
deletingEffects sas a = concatMap f (SAS.actionPost a)
  where
    getPre :: Int -> Maybe SAS.Fact
    getPre var = find (\(var', _) -> var == var') (SAS.actionPre a)
    -- get the deleting effects for the variable var
    f (var, val) = case getPre var of
      Just (_, val') -> [convertFact sas (var, val')]
      Nothing -> [convertFact sas (var, val') | val' <- [0 .. SAS.domainSize sas var - 1], val /= val']

convertAction :: SAS -> SAS.Action -> Action
convertAction sas a@(SAS.Action name pre post c) = Action name pre' add del c
  where
    pre' = convertFacts sas pre
    add = convertFacts sas post
    del = deletingEffects sas a

fromSAS :: SAS -> PlanningTask NoConstraint
fromSAS sas@(SAS.SAS _ mgs s g as) = PlanningTask nfacts showfact as' s' g' NoConstraint (oldMGs ++ newMGs)
  where
    nfacts = perfectHash sas Vec.! SAS.nVars sas
    showfact fact = SAS.showFact sas $ inverseHash sas nfacts Vec.! fact
    as' = map (convertAction sas) as
    s' = convertFacts sas (zip [0 ..] s)
    g' = convertFacts sas g
    oldMGs = map (convertFacts sas) mgs
    newMGs =
      [ [convertFact sas (var, val) | val <- [0 .. SAS.domainSize sas var - 1]]
        | var <- [0 .. SAS.nVars sas - 1]
      ]