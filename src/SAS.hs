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
    nVars,
    showFact,
    perfectHash,
    nFacts,
    nameToFact
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.Map as Map

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

nVars :: SAS -> Int
nVars sas = length $ variables sas

-- Compute the hash of v = 0 for all SAS variables v. The last element is the number of facts
perfectHash :: SAS -> Vector Int
perfectHash sas = Vec.unfoldrExactN (SAS.nVars sas + 1) f (0, 0)
  where
    -- f iterates through all variables v and computes the hash for v = 0,
    -- while remembering number of the variable and hash value
    f (var, hash) = (hash, (var + 1, hash + SAS.domainSize sas var))

nFacts :: SAS -> Int
nFacts sas = perfectHash sas Vec.! SAS.nVars sas

nameFact :: SAS -> Fact -> ByteString
nameFact sas (var, val) = varValues (variable sas var) Vec.! val

showFact :: SAS -> Fact -> ByteString
showFact sas (var, val) =
  C8.concat
    [varName (variables sas Vec.! var), C8.pack " = ", nameFact sas (var, val)]

facts :: SAS -> [Fact]
facts sas = [(var, val) | var <- [0..nVars sas - 1], val <- [0.. domainSize sas var - 1]]

nameToFact :: SAS -> ByteString -> Fact
nameToFact sas varval = Map.fromList (map (\f -> (nameFact sas f, f)) $ facts sas) Map.! varval