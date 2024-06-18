module Types
  ( Fact (Fact),
    MutexGroup(MutexGroup),
    State,
    Goal,
    Action (..),
    Time,
    Variable (..),
    Plan (..),
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (intercalate)

--newtype Fact = Fact Int deriving (Eq, Ord, Show)
newtype Fact = Fact ByteString deriving (Eq, Ord, Show)

newtype MutexGroup = MutexGroup [Fact]

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

instance Show Action where
  show a = C8.unpack $ actionName a

type Time = Int

data Variable = ActionV Time Action | FactV Time Fact | AtMostOneActionV Time Int | AtMostOnceV Fact Int
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