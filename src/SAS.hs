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
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
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
variable pt var = variables pt Vec.! var

domainSize :: SAS -> Int -> Int
domainSize sas var = length $ varValues (variable sas var)

nVars :: SAS -> Int
nVars sas = length $ variables sas

getVarVal :: SAS -> Int -> Int -> ByteString
getVarVal pt var val = varValues (variable pt var) Vec.! val

showFact :: SAS -> Fact -> ByteString
showFact pt (var, val) =
  C8.concat
    [varName (variables pt Vec.! var), C8.pack " = ", getVarVal pt var val]