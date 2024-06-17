module Types
  ( Fact,
    MutexGroup,
    State,
    Goal,
    Action (..),
    PlanningTask (..),
    Time,
    Variable (..),
    Plan (..),
    ptFacts,
    ptNumberActions,
    ptShowFacts,
    ptShowAction,
    printPlanningTask
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (intercalate)

type Fact = Int

type MutexGroup = [Fact]

type State = [Fact]

type Goal = [Fact]

data Action = Action
  { actionName :: ByteString,
    actionPre :: [Fact],
    actionAdd :: [Fact],
    actionDel :: [Fact],
    actionCost :: Int
  }
  deriving (Eq, Ord)

data PlanningTask constraintType = PlanningTask
  { ptNumberFacts :: Int,
    ptShowFact :: Fact -> ByteString,
    ptActions :: [Action],
    ptInitalState :: State,
    ptGoal :: Goal,
    ptConstraints :: constraintType Fact,
    ptMutexGroups :: [MutexGroup]
  }

instance Show Action where
  show a = C8.unpack $ actionName a

ptFacts :: PlanningTask a -> [Fact]
ptFacts pt = [0 .. ptNumberFacts pt - 1]

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

printPlanningTask :: PlanningTask a -> IO ()
printPlanningTask pt =
  C8.putStrLn $
    C8.concat
      [ C8.pack "There are ",
        C8.pack (show (ptNumberFacts pt)),
        C8.pack " facts:\n",
        C8.pack "\nActions:\n",
        C8.unlines (map (ptShowAction pt) (ptActions pt)),
        C8.pack "Initial state:\n",
        C8.unlines $ map (ptShowFact pt) (ptInitalState pt),
        C8.pack "Goal:\n",
        ptShowFacts pt (ptGoal pt),
        C8.pack "\nMutex Groups:\n",
        C8.unlines (map (ptShowFacts pt) (ptMutexGroups pt))
      ]

type Time = Int

data Variable = ActionV Time Action | FactV Time Fact | AtMostOneV Time Int
  deriving (Eq, Ord, Show)

newtype Plan = Plan [Action]

instance Show Plan where
  show (Plan as) =
    "Plan with length "
      ++ show (length as)
      ++ " and cost "
      ++ show (sum $ map actionCost as)
      ++ ":\n  "
      ++ intercalate "\n  " (map show as)