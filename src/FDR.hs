module FDR
  ( Variable (..),
    Fact,
    MutexGroup,
    State,
    Goal,
    Action (..),
    FDR (..),
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

data FDR = FDR
  { variables :: Vector Variable,
    mutexGroups :: [MutexGroup],
    initialState :: State,
    goal :: Goal,
    actions :: [Action]
  }

instance Show Variable where
  show (Var name vals) = show name ++ " in " ++ show vals

variable :: FDR -> Int -> Variable
variable pt var = variables pt Vec.! var

domainSize :: FDR -> Int -> Int
domainSize fdr var = length $ varValues (variable fdr var)

nVars :: FDR -> Int
nVars fdr = length $ variables fdr

getVarVal :: FDR -> Int -> Int -> ByteString
getVarVal pt var val = varValues (variable pt var) Vec.! val

getStateVals :: FDR -> State -> [ByteString]
getStateVals pt s =
  let f var i = varValues var Vec.! i
   in zipWith f (Vec.toList $ variables pt) s

printState :: FDR -> State -> IO ()
printState pt s = foldMap C8.putStrLn $ getStateVals pt s

showFact :: FDR -> Fact -> ByteString
showFact pt (var, val) =
  C8.concat
    [varName (variables pt Vec.! var), C8.pack " = ", getVarVal pt var val]