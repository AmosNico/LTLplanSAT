{-# LANGUAGE OverloadedStrings #-}

module Basic
  ( Atom (..),
    showAtom,
    showAtoms,
    Fact (..),
    showFact,
    showFacts,
    negateFact,
    isPosAtom,
    factToAtom,
    MutexGroup (MutexGroup),
    State,
    Goal,
    Action (..),
    showAction,
    Time,
    Variable (..),
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Foldable as Set
import Data.Set (Set)

newtype Atom = Atom ByteString deriving (Eq, Ord, Show)

showAtom :: Atom -> ByteString
showAtom (Atom name) = name

showAtoms :: [Atom] -> ByteString
showAtoms atoms = C8.intercalate (C8.pack ", ") $ map showAtom atoms

data Fact = PosAtom Atom | NegAtom Atom deriving (Eq, Ord)

showFact :: Fact -> ByteString
showFact (PosAtom atom) = showAtom atom
showFact (NegAtom atom) = C8.append "not " (showAtom atom)

instance Show Fact where
  show = C8.unpack . showFact

showFacts :: [Fact] -> ByteString
showFacts fs = C8.intercalate ", " $ map showFact fs

negateFact :: Fact -> Fact
negateFact (PosAtom atom) = NegAtom atom
negateFact (NegAtom atom) = PosAtom atom

isPosAtom :: Fact -> Bool
isPosAtom (PosAtom _) = True
isPosAtom (NegAtom _) = False

factToAtom :: Fact -> Atom
factToAtom (PosAtom atom) = atom
factToAtom (NegAtom atom) = atom

newtype MutexGroup = MutexGroup [Fact]

type State = [Fact]

type Goal = [Fact]

data Action = Action
  { actionName :: ByteString,
    actionPre :: Set Fact,
    actionPost :: Set Fact,
    actionCost :: Int
  }

-- No need to compare preconditions and postconditions
instance Eq Action where
  (==) a1 a2 = actionName a1 == actionName a2

instance Ord Action where
  compare a1 a2 = compare (actionName a1) (actionName a2)

instance Show Action where
  show a = C8.unpack $ actionName a

showAction :: Action -> ByteString
showAction (Action name pre post cost) =
  C8.concat
    [ name,
      " (cost ",
      C8.pack (show cost),
      "): \n  pre = {",
      showFacts $ Set.toList pre, -- TODO
      "}: \n  post = {",
      showFacts $ Set.toList post, -- TODO
      "}"
    ]

type Time = Int

data Variable = ActionVar Time Action | AtomVar Time Atom
  deriving (Eq, Ord, Show)