module PlanningTask
  ( PlanningTask (..),
    ptNumberFacts,
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
import Types (Action (Action), Fact (Fact), Goal, MutexGroup (MutexGroup), State)

data PlanningTask constraintType = PlanningTask
  { ptFacts :: [Fact],
    ptActions :: [Action],
    ptInitalState :: State,
    ptGoal :: Goal,
    ptConstraints :: constraintType Fact,
    ptMutexGroups :: [MutexGroup]
  }

ptNumberFacts :: PlanningTask a -> Int
ptNumberFacts pt = length $ ptFacts pt

ptNumberActions :: PlanningTask a -> Int
ptNumberActions pt = length $ ptActions pt

showFact :: Fact -> ByteString
showFact (Fact fact) = fact

showFacts :: [Fact] -> ByteString
showFacts fs = C8.intercalate (C8.pack ", ") $ map showFact fs

showAction :: Action -> ByteString
showAction (Action name pre add del cost) =
  C8.concat
    [ name,
      C8.pack " (cost ",
      C8.pack (show cost),
      C8.pack "): \n  pre = {",
      showFacts pre,
      C8.pack "}, \n  post = {",
      showFacts add,
      C8.pack "}, \n  del = {",
      showFacts del,
      C8.pack "}"
    ]

printPlanningTask :: (Constraints a) => PlanningTask a -> IO ()
printPlanningTask pt =
  C8.putStrLn $
    C8.concat
      [ C8.pack "There are ",
        C8.pack (show (ptNumberFacts pt)),
        C8.pack " facts:\n",
        showFacts (ptFacts pt),
        C8.pack "\nActions:\n",
        C8.unlines (map showAction (ptActions pt)),
        C8.pack "Initial state:\n",
        C8.unlines $ map showFact (ptInitalState pt),
        C8.pack "Goal:\n",
        showFacts (ptGoal pt),
        showConstraints showFact (ptConstraints pt),
        C8.pack "\nMutex Groups:\n",
        C8.unlines (map (showFacts . \(MutexGroup facts) -> facts) (ptMutexGroups pt))
      ]

convertFact :: SAS -> SAS.Fact -> Fact
convertFact sas fact = Fact $ SAS.factName sas fact

convertFacts :: SAS -> [SAS.Fact] -> [Fact]
convertFacts sas = map (convertFact sas)

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

fromSAS :: (Constraints c) => SAS -> c ByteString -> PlanningTask c
fromSAS sas constraints = PlanningTask facts actions initial goal constraints' (oldMGs ++ newMGs)
  where
    facts = convertFacts sas $ SAS.facts sas
    actions = map (convertAction sas) $ SAS.actions sas
    initial = convertFacts sas (zip [0 ..] $ SAS.initialState sas)
    goal = convertFacts sas $ SAS.goal sas
    constraints' = fmap Fact constraints
    oldMGs = map (MutexGroup . convertFacts sas) $ SAS.mutexGroups sas
    newMGs =
      [ MutexGroup [convertFact sas (var, val) | val <- [0 .. SAS.domainSize sas var - 1]]
        | var <- [0 .. SAS.nVars sas - 1]
      ]
