module Parsing (Fact,Variable(..),MutexGroup,State,Goal,Action(..),PlanningTask(..),printState,printFacts,readSAS,toInt) where
import Control.Monad (replicateM)
import Data.List (sort) 
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Control.Exception (assert)
import Data.String (String)

type Fact = (Int,Int)
type Effect = (Fact,Fact)
data Variable = Var
    { getVarName :: ByteString
    , getVals :: [ByteString]
    }
type MutexGroup = [Fact]
type State = Vector Int
type Goal = Vector Fact
data Action = Action
    { getActionName :: ByteString
    , getPre :: Vector Fact
    , getPost :: Vector Fact
    , getCost :: Int
    }
data PlanningTask = PT
    { getVars :: Vector Variable
    , getMGs :: [MutexGroup]
    , getState :: State
    , getGoal :: Goal
    , getActions :: [Action]
    }

expect :: String -> IO ()
expect s = do
    input <- C8.getLine
    assert (input == C8.pack s) (return ())


getVarVal :: PlanningTask -> Int -> Int -> ByteString
getVarVal pt var val = getVals (getVars pt Vec.! var) !! val

getStateVals :: PlanningTask -> State -> [ByteString]
getStateVals pt s =
    let f var i = getVals var !! i in
    Vec.toList $ Vec.zipWith f (getVars pt) s

printState :: PlanningTask -> State -> IO ()
printState pt s = foldMap C8.putStrLn $ getStateVals pt s

showFact :: PlanningTask -> Fact -> ByteString
showFact pt (var,val) = C8.append (C8.pack (show var ++ " ")) (getVarVal pt var val)

printFacts :: PlanningTask -> Vec.Vector Fact -> IO ()
printFacts pt = foldMap (C8.putStrLn . showFact pt)

readInt :: IO Int
readInt = do
    _ <- C8.getLine
    n <- C8.getLine
    _ <- C8.getLine
    return (toInt n)

readVariable :: IO Variable
readVariable = do
    expect "begin_variable"
    name <- C8.getLine
    _ <- C8.getLine
    len <- readLn
    vals <- replicateM len C8.getLine
    expect "end_variable"
    pure $ Var name vals

toInt :: ByteString -> Int
toInt = fst. fromJust . C8.readInt

-- unsafe
toFact :: C8.ByteString -> Fact
toFact s =
    let [var,val] = C8.words s in
    (toInt var, toInt val)

readFacts :: IO [Fact]
readFacts = do
    len <- C8.getLine
    strings <- replicateM (toInt len) C8.getLine
    return (map toFact strings)

readMutexGroup :: IO MutexGroup
readMutexGroup = do
    _ <- C8.getLine
    mg <- readFacts
    _ <- C8.getLine
    return mg

readState :: Int -> IO State
readState n = do
    _ <- C8.getLine
    state <- replicateM n C8.getLine
    _ <- C8.getLine
    return $ Vec.fromList (map toInt state)

readGoal :: IO Goal
readGoal = do
    _ <- C8.getLine
    goal <- readFacts
    _ <- C8.getLine
    return $ Vec.fromList goal

-- unsafe
toEffect :: ByteString -> Effect
toEffect s =
    let [_, var,pre,post] = C8.words s in
    ((toInt var, toInt pre),(toInt var, toInt post))

readEffects :: IO [Effect]
readEffects = do
    len <- C8.getLine
    strings <- replicateM (toInt len) C8.getLine
    return (map toEffect strings)

readAction :: IO Action
readAction = do
    _ <- C8.getLine
    name <- C8.getLine
    pre1 <- readFacts
    effects <- readEffects
    cost <- C8.getLine
    _ <- C8.getLine
    let pre2 = filter (\p -> snd p /= (-1)) $ map fst effects
    -- sort first extracts all increasing/decreasing parts of the list, and merges them
    let pre = Vec.fromList $ sort $ pre1 ++ pre2
    let post = Vec.fromList $ sort $ map snd effects
    return (Action name pre post (toInt cost))

readSAS :: IO PlanningTask
readSAS = do
    _ <- readInt -- version
    _ <- readInt -- metric
    nVars <- C8.getLine
    vars <- replicateM (toInt nVars) readVariable
    nMutexGroup <- C8.getLine
    mutexGroups <- replicateM (toInt nMutexGroup) readMutexGroup
    state <- readState (toInt nVars)
    goal <- readGoal
    nAction <- C8.getLine
    actions <- replicateM (toInt nAction) readAction
    _ <- C8.getLine
    return (PT (Vec.fromList vars) mutexGroups state goal actions)


main :: IO ()
main = do
    pt <- readSAS
    command <- getLine
    case command of
        "variable" -> do
            l <- C8.getLine
            let (var,val) = toFact l
            C8.putStrLn (getVals (getVars pt Vec.! var) !! val)
        "state" -> printState pt (getState pt)
        "goal" -> printFacts pt (getGoal pt)
        "actionname" -> do
            action <- readLn
            C8.putStrLn (getActionName (getActions pt !! action))
        "actioncost" -> do
            action <- readLn
            print (getCost (getActions pt !! action))
        "precondition" -> do
            action <- readLn
            printFacts pt (getPre (getActions pt !! action))
        "effect" -> do
            action <- readLn
            printFacts pt (getPost (getActions pt !! action))
        _ -> return ()

