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
import Data.List (find)
import SAS (SAS)
import qualified SAS
import Types
import qualified Data.Set as Set

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
      [ C8.pack "There are ",
        C8.pack (show (ptNumberAtoms pt)),
        C8.pack " atoms:\n",
        showAtoms (ptAtoms pt),
        C8.pack "\nActions:\n",
        C8.unlines (map showAction (ptActions pt)),
        C8.pack "Initial state:\n",
        C8.unlines $ map showFact (ptInitalState pt),
        C8.pack "Goal:\n",
        showFacts (ptGoal pt),
        showConstraints (ptConstraints pt),
        C8.pack "\nMutex Groups:\n",
        C8.unlines (map (showFacts . \(MutexGroup facts) -> facts) (ptMutexGroups pt))
      ]

nameToFact :: ByteString -> Fact
nameToFact name
  | C8.pack "Atom " `C8.isPrefixOf` name = PosAtom $ Atom $ C8.drop 5 name
  | C8.pack "NegatedAtom " `C8.isPrefixOf` name = NegAtom $ Atom $ C8.drop 12 name
  | otherwise =
      error $
        "Error while converting the SAS-description to PlanningTask. The SAS-fact "
          ++ show name
          ++ " is expected to start with \"Atom \" or \"NegatedAtom\"."

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


fromSAS :: SAS -> c -> PlanningTask c
fromSAS sas constraints = PlanningTask atoms actions initial goal constraints (oldMGs ++ newMGs)
  where
    -- remove possible duplicate facts. This is probably not necaissary.
    atoms = Set.toList $ Set.fromList $ map factToAtom $ convertFacts sas $ SAS.facts sas
    actions = map (convertAction sas) $ SAS.actions sas
    initial = convertFacts sas (zip [0 ..] $ SAS.initialState sas)
    goal = convertFacts sas $ SAS.goal sas
    oldMGs = map (MutexGroup . convertFacts sas) $ SAS.mutexGroups sas
    newMGs =
      [ MutexGroup [convertFact sas (var, val) | val <- [0 .. SAS.domainSize sas var - 1]]
        | var <- [0 .. SAS.nVars sas - 1]
      ]
