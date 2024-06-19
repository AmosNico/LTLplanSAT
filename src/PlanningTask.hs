{-# LANGUAGE OverloadedStrings #-}

module PlanningTask
  ( PlanningTask (..),
    ptNumberAtoms,
    ptNumberActions,
    showFact,
    showFacts,
    showAction,
    printPlanningTask,
    fromSAS,
  )
where

import Constraints (Constraints (showConstraints))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (find, sort, (\\))
import qualified Data.List.NonEmpty as NonEmpty
import SAS (SAS)
import qualified SAS
import Types

data PlanningTask constraintType = PlanningTask
  { ptAtoms :: [Atom],
    ptActions :: [Action],
    ptInitalState :: State,
    ptGoal :: Goal,
    ptConstraints :: constraintType,
    ptMutexGroups :: [MutexGroup]
  }

ptNumberAtoms :: PlanningTask a -> Int
ptNumberAtoms pt = length $ ptAtoms pt

ptNumberActions :: PlanningTask a -> Int
ptNumberActions pt = length $ ptActions pt

printPlanningTask :: (Constraints a) => PlanningTask a -> IO ()
printPlanningTask pt =
  C8.putStrLn $
    C8.concat
      [ "There are ",
        C8.pack (show (ptNumberAtoms pt)),
        " atoms:\n",
        showAtoms (ptAtoms pt),
        "\nActions:\n",
        C8.unlines (map showAction (ptActions pt)),
        "Initial state:\n",
        C8.unlines $ map showFact (ptInitalState pt),
        "Goal:\n",
        showFacts (ptGoal pt),
        showConstraints (ptConstraints pt),
        "\nMutex Groups:\n",
        C8.unlines (map (showFacts . \(MutexGroup facts) -> facts) (ptMutexGroups pt))
      ]

nameToFact :: ByteString -> Fact
nameToFact name
  | "Atom " `C8.isPrefixOf` name = PosAtom $ Atom $ C8.drop 5 name
  | "NegatedAtom " `C8.isPrefixOf` name = NegAtom $ Atom $ C8.drop 12 name
  | otherwise =
      error $
        "Error while converting the SAS-description to PlanningTask. The SAS-fact "
          ++ show name
          ++ " is expected to start with \"Atom \" or \"NegatedAtom\"."

factToName :: Fact -> ByteString
factToName (PosAtom (Atom name)) = C8.append "Atom " name
factToName (NegAtom (Atom name)) = C8.append "NegatedAtom " name

convertFact :: SAS -> SAS.Fact -> Fact
convertFact sas fact = nameToFact $ SAS.factName sas fact

convertFacts :: SAS -> [SAS.Fact] -> [Fact]
convertFacts sas = map $ convertFact sas

deletingEffects :: SAS -> SAS.Action -> [Fact]
deletingEffects sas a = convertFacts sas $ concatMap f (SAS.actionPost a)
  where
    getPre :: Int -> Maybe SAS.Fact
    getPre var = find (\(var', _) -> var == var') (SAS.actionPre a)
    -- get the deleting effects for the variable var
    f (var, val) = case getPre var of
      Just (_, val') -> [(var, val')]
      Nothing -> [(var, val') | val' <- [0 .. SAS.domainSize sas var - 1], val /= val']

convertAction :: SAS -> SAS.Action -> Action
convertAction sas a@(SAS.Action name pre post c) =
  Action name pre' (post' ++ del) c
  where
    pre' = convertFacts sas pre
    post' = convertFacts sas post
    del = map negateFact $ deletingEffects sas a

removeDuplicates :: (Ord a) => [a] -> [a]
removeDuplicates = map NonEmpty.head . NonEmpty.group . sort

convertInitialState :: SAS -> State
convertInitialState sas = removeDuplicates (init1 ++ init2)
  where
    init' = zip [0 ..] $ SAS.initialState sas
    init1 = convertFacts sas init'
    init2 = map negateFact $ convertFacts sas $ SAS.facts sas \\ init'

fromSAS :: SAS -> c -> PlanningTask c
fromSAS sas constraints = PlanningTask atoms actions initial goal constraints (oldMGs ++ newMGs)
  where
    -- Note: not all facts correspond to facts in SAS, and every atom corresponds to one or two facts in SAS
    atoms = removeDuplicates $ map factToAtom $ convertFacts sas $ SAS.facts sas
    actions = map (convertAction sas) $ SAS.actions sas
    initial = convertInitialState sas
    goal = convertFacts sas $ SAS.goal sas
    oldMGs = map (MutexGroup . convertFacts sas) $ SAS.mutexGroups sas
    newMGs =
      [ MutexGroup [convertFact sas (var, val) | val <- [0 .. SAS.domainSize sas var - 1]]
        | var <- [0 .. SAS.numberVariables sas - 1]
      ]
