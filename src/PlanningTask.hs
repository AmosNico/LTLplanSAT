module PlanningTask
  ( PlanningTask (..),
    ptFacts,
    ptNumberActions,
    ptShowFacts,
    ptShowAction,
    printPlanningTask,
    fromSAS,
  )
where

import Constraints (Constraints (showConstraints))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (find)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import SAS (SAS)
import qualified SAS
import Types (Action (Action), Fact (Fact), Goal, MutexGroup (MutexGroup), State)

data PlanningTask constraintType = PlanningTask
  { ptNumberFacts :: Int,
    ptShowFact :: Fact -> ByteString,
    ptActions :: [Action],
    ptInitalState :: State,
    ptGoal :: Goal,
    ptConstraints :: constraintType Fact,
    ptMutexGroups :: [MutexGroup]
  }

ptFacts :: PlanningTask a -> [Fact]
ptFacts pt = map Fact [0 .. ptNumberFacts pt - 1]

ptNumberActions :: PlanningTask a -> Int
ptNumberActions pt = length $ ptActions pt

ptShowFacts :: PlanningTask a -> [Fact] -> ByteString
ptShowFacts pt fs = C8.intercalate (C8.pack ", ") $ map (ptShowFact pt) fs

ptShowAction :: PlanningTask a -> Action -> ByteString
ptShowAction pt (Action name pre add del cost) =
  C8.concat
    [ name,
      C8.pack " (cost ",
      C8.pack (show cost),
      C8.pack "): \n  pre = {",
      ptShowFacts pt pre,
      C8.pack "}, \n  post = {",
      ptShowFacts pt add,
      C8.pack "}, \n  del = {",
      ptShowFacts pt del,
      C8.pack "}"
    ]

printPlanningTask :: (Constraints a) => PlanningTask a -> IO ()
printPlanningTask pt =
  C8.putStrLn $
    C8.concat
      [ C8.pack "There are ",
        C8.pack (show (ptNumberFacts pt)),
        C8.pack " facts:\n",
        ptShowFacts pt (ptFacts pt),
        C8.pack "\nActions:\n",
        C8.unlines (map (ptShowAction pt) (ptActions pt)),
        C8.pack "Initial state:\n",
        C8.unlines $ map (ptShowFact pt) (ptInitalState pt),
        C8.pack "Goal:\n",
        ptShowFacts pt (ptGoal pt),
        showConstraints (ptShowFact pt) (ptConstraints pt),
        C8.pack "\nMutex Groups:\n",
        C8.unlines (map (ptShowFacts pt . \(MutexGroup facts) -> facts) (ptMutexGroups pt))
      ]

convertFact :: SAS -> SAS.Fact -> Fact
convertFact sas (var, val) = Fact $ (SAS.perfectHash sas Vec.! var) + val

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

fromSAS :: (Constraints c) => SAS -> c ByteString -> PlanningTask c
fromSAS sas constraints = PlanningTask nFacts showFact actions initial goal constraints' (oldMGs ++ newMGs)
  where
    nFacts = SAS.nFacts sas
    showFact (Fact fact) = SAS.showFact sas $ inverseHash sas nFacts Vec.! fact
    actions = map (convertAction sas) $ SAS.actions sas
    initial = convertFacts sas (zip [0 ..] $ SAS.initialState sas)
    goal = convertFacts sas $ SAS.goal sas
    constraints' = fmap (convertFact sas . SAS.nameToFact sas) constraints
    oldMGs = map (MutexGroup . convertFacts sas) $ SAS.mutexGroups sas
    newMGs =
      [ MutexGroup [convertFact sas (var, val) | val <- [0 .. SAS.domainSize sas var - 1]]
        | var <- [0 .. SAS.nVars sas - 1]
      ]
