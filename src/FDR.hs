module FDR (Variable(..), Fact, MutexGroup, State, Goal, Action(..), FDR(..)) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Vector (Vector)
import qualified Data.Vector as Vec

data Variable = Var
    { getVarName :: ByteString
    , getVals :: [ByteString]
    }
type Fact = (Int,Int)
type MutexGroup = [Fact]
type State = Vector Int
type Goal = Vector Fact
data Action = Action
    { getActionName :: ByteString
    , getPre :: Vector Fact
    , getPost :: Vector Fact
    , getCost :: Int
    }
data FDR = FDR
    { getVars :: Vector Variable
    , getMGs :: [MutexGroup]
    , getState :: State
    , getGoal :: Goal
    , getActions :: [Action]
    }
    
getVarVal :: FDR -> Int -> Int -> ByteString
getVarVal pt var val = getVals (getVars pt Vec.! var) !! val

getStateVals :: FDR -> State -> [ByteString]
getStateVals pt s =
    let f var i = getVals var !! i in
    Vec.toList $ Vec.zipWith f (getVars pt) s

printState :: FDR -> State -> IO ()
printState pt s = foldMap C8.putStrLn $ getStateVals pt s

showFact :: FDR -> Fact -> ByteString
showFact pt (var,val) = C8.append (C8.pack (show var ++ " ")) (getVarVal pt var val)

printFacts :: FDR -> Vec.Vector Fact -> IO ()
printFacts pt = foldMap (C8.putStrLn . showFact pt)