module Types
  ( Atom (..),
    showAtom,
    showAtoms,
    Fact (..),
    showFact,
    showFacts,
    negateFact,
    isPosAtom,
    factToAtom,
    MutexGroup(MutexGroup),
    State,
    Goal,
    Action (..),
    showAction,
    Time,
    Variable (..),
    Plan (..),
  )
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (intercalate)

--newtype Fact = Fact Int deriving (Eq, Ord, Show)
newtype Atom = Atom ByteString deriving (Eq, Ord, Show)

showAtom :: Atom -> ByteString
showAtom (Atom name) = name

showAtoms :: [Atom] -> ByteString
showAtoms atoms = C8.intercalate (C8.pack ", ") $ map showAtom atoms

data Fact = PosAtom Atom | NegAtom Atom deriving (Eq, Ord, Show)

showFact :: Fact -> ByteString
showFact (PosAtom atom) = showAtom atom
showFact (NegAtom atom) = C8.append (C8.pack "not ") (showAtom atom)

showFacts :: [Fact] -> ByteString
showFacts fs = C8.intercalate (C8.pack ", ") $ map showFact fs

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
    actionPre :: [Fact],
    actionPost :: [Fact],
    actionCost :: Int
  }
  deriving (Eq, Ord)

showAction :: Action -> ByteString
showAction (Action name pre post cost) =
  C8.concat
    [ name,
      C8.pack " (cost ",
      C8.pack (show cost),
      C8.pack "): \n  pre = {",
      showFacts pre,
      C8.pack "}: \n  post = {",
      showFacts post,
      C8.pack "}"
    ]

instance Show Action where
  show a = C8.unpack $ actionName a

type Time = Int

data Variable = ActionV Time Action | AtomV Time Atom | AtMostOneActionV Time Int | AtMostOnceV Fact Int
  deriving (Eq, Ord)

newtype Plan = Plan [Action]

instance Show Plan where
  show (Plan as) =
    "Plan with length "
      ++ show (length as)
      ++ " and cost "
      ++ show (sum $ map actionCost as)
      ++ ":\n  "
      ++ intercalate "\n  " (map show as)