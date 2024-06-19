module SAS
  ( Variable (..),
    Fact,
    MutexGroup,
    State,
    Goal,
    Action (..),
    SAS (..),
    domainSize,
    variable,
    numberVariables,
    perfectHash,
    numberFacts,
    factName,
    facts,
  )
where

import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Vector as Vec

data Variable = Var
  { varName :: ByteString,
    varValues :: Vector ByteString
  }

type Fact = (Int, Int)

type MutexGroup = [Fact]

type State = [Int]

type Goal = [Fact]

data Action = Action
  { actionName :: ByteString,
    actionPre :: [Fact],
    actionPost :: [Fact],
    actionCost :: Int
  }

data SAS = SAS
  { variables :: Vector Variable,
    mutexGroups :: [MutexGroup],
    initialState :: State,
    goal :: Goal,
    actions :: [Action]
  }

instance Show Variable where
  show (Var name vals) = show name ++ " in " ++ show vals

variable :: SAS -> Int -> Variable
variable sas var = variables sas Vec.! var

domainSize :: SAS -> Int -> Int
domainSize sas var = length $ varValues (variable sas var)

numberVariables :: SAS -> Int
numberVariables sas = length $ variables sas

-- Compute the hash of v = 0 for all SAS variables v. The last element is the number of facts
perfectHash :: SAS -> Vector Int
perfectHash sas = Vec.unfoldrExactN (numberVariables sas + 1) f (0, 0)
  where
    -- f iterates through all variables v and computes the hash for v = 0,
    -- while remembering number of the variable and hash value
    f (var, hash) = (hash, (var + 1, hash + domainSize sas var))

numberFacts :: SAS -> Int
numberFacts sas = perfectHash sas Vec.! numberVariables sas

factName :: SAS -> Fact -> ByteString
factName sas (var, val) = varValues (variable sas var) Vec.! val

facts :: SAS -> [Fact]
facts sas = [(var, val) | var <- [0 .. numberVariables sas - 1], val <- [0 .. domainSize sas var - 1]]