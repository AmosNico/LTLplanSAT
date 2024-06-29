module Basic
  ( Atom (..),
    Fact (..),
    negateFact,
    isPosAtom,
    factToAtom,
    showNamedList,
    MutexGroup (..),
    State,
    Goal,
    Action (..),
    showActionName,
    Time,
    Variable (..),
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

isPosAtom :: Fact -> Bool
isPosAtom (PosAtom _) = True
isPosAtom (NegAtom _) = False

factToAtom :: Fact -> Atom
factToAtom (PosAtom atom) = atom
factToAtom (NegAtom atom) = atom

indent :: String -> String
indent = unlines . map ("  " ++) . lines

showNamedList :: (Show a) => String -> [a] -> String
showNamedList name xs = name ++ "\n" ++ concatMap showItem xs
  where
    showItem x = indent $ show x

newtype MutexGroup = MutexGroup [Fact]

instance Show MutexGroup where
  show (MutexGroup facts) = showNamedList "Mutex group" facts

type State = [Fact]

type Goal = [Fact]

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

type Time = Int

data Variable = ActionVar Time Action | AtomVar Time Atom
  deriving (Eq, Ord)

instance Show Variable where
  show (ActionVar t action) = "ActionVariable " ++ show t ++ " " ++ showActionName action
  show (AtomVar t atom) = "AtomVariable " ++ show t ++ " " ++ show atom