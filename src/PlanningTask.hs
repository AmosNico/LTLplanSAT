module PlanningTask
  ( Atom (..),
    Fact (..),
    negateFact,
    factToAtom,
    showNamedList,
    MutexGroup (..),
    State (..),
    Goal (..),
    Action (..),
    showActionName,
    PlanningTask (..),
    ptNumberAtoms,
    ptNumberActions,
    ptFacts,
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Set (Set)
import qualified Data.Set as Set

newtype Atom = Atom ByteString deriving (Eq, Ord)

instance Show Atom where
  show (Atom name) = C8.unpack name

data Fact = PosAtom Atom | NegAtom Atom deriving (Eq, Ord)

instance Show Fact where
  show (PosAtom atom) = show atom
  show (NegAtom atom) = "Not " ++ show atom

negateFact :: Fact -> Fact
negateFact (PosAtom atom) = NegAtom atom
negateFact (NegAtom atom) = PosAtom atom

factToAtom :: Fact -> Atom
factToAtom (PosAtom atom) = atom
factToAtom (NegAtom atom) = atom

indent :: String -> String
indent = unlines . map ("  " ++) . lines

showNamedList :: (Show a) => String -> [a] -> String
showNamedList name xs = name ++ "\n" ++ concatMap showItem xs
  where
    showItem x = indent $ show x

newtype MutexGroup = MutexGroup {mutexGroupFacts :: [Fact]}

instance Show MutexGroup where
  show (MutexGroup facts) = showNamedList "Mutex group" facts

newtype State = State {stateFacts :: [Fact]}

instance Show State where
  show (State facts) = showNamedList "Initial state:" facts

newtype Goal = Goal {goalFacts :: [Fact]}

instance Show Goal where
  show (Goal facts) = showNamedList "Goal:" facts

data Action = Action
  { actionName :: ByteString,
    actionPre :: Set Fact,
    actionPost :: Set Fact,
    actionCost :: Int
  }

showActionName :: Action -> String
showActionName a = C8.unpack $ actionName a

-- Assuming uniqueness of names, there is no need to compare preconditions and postconditions
instance Eq Action where
  (==) a1 a2 = actionName a1 == actionName a2

instance Ord Action where
  compare a1 a2 = compare (actionName a1) (actionName a2)

instance Show Action where
  show (Action name pre post cost) =
    C8.unpack name
      ++ " (cost "
      ++ show cost
      ++ "):\n"
      ++ indent (showNamedList "pre:" $ Set.toList pre)
      ++ indent (showNamedList "post:" $ Set.toList post)

-- c is the type of constraints
data PlanningTask c = PlanningTask
  { ptAtoms :: [Atom],
    ptActions :: [Action],
    ptInitalState :: State,
    ptGoal :: Goal,
    ptConstraints :: c,
    ptMutexGroups :: [MutexGroup]
  }

ptNumberAtoms :: PlanningTask c -> Int
ptNumberAtoms pt = length $ ptAtoms pt

ptNumberActions :: PlanningTask c -> Int
ptNumberActions pt = length $ ptActions pt

ptFacts :: PlanningTask c -> [Fact]
ptFacts pt = map PosAtom (ptAtoms pt) ++ map NegAtom (ptAtoms pt)

instance (Show c) => Show (PlanningTask c) where
  show pt =
    showNamedList ("There are " ++ show (ptNumberAtoms pt) ++ " atoms:") (ptAtoms pt)
      ++ showNamedList "Actions:" (ptActions pt)
      ++ show (ptInitalState pt)
      ++ show (ptGoal pt)
      ++ show (ptConstraints pt)
      ++ showNamedList "Mutex Groups:" (ptMutexGroups pt)