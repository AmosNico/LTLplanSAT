module ParseSAS (readSAS,toInt) where

import qualified FDR

import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Control.Monad (unless, replicateM)

expect :: String -> IO ()
expect s = do
    input <- C8.getLine
    unless (input == C8.pack s) $ putStrLn $
        "Unexpected input while parsing SAS file: Expected "
        ++ show s ++ ", but found " ++ show input ++ "."

readVersion :: IO Int
readVersion = do
    expect "begin_version"
    n <- C8.getLine
    expect "end_version"
    return (toInt n)

readMetric :: IO Int
readMetric = do
    expect "begin_metric"
    n <- C8.getLine
    expect "end_metric"
    return (toInt n)

readVariable :: IO FDR.Variable
readVariable = do
    expect "begin_variable"
    name <- C8.getLine
    _ <- C8.getLine
    len <- readLn
    vals <- replicateM len C8.getLine
    expect "end_variable"
    pure $ FDR.Var name (Vec.fromList vals)

toInt :: ByteString -> Int
toInt = fst. fromJust . C8.readInt

-- unsafe
toFact :: C8.ByteString -> FDR.Fact
toFact s =
    let [var, val] = C8.words s in
    (toInt var, toInt val)

readFacts :: IO [FDR.Fact]
readFacts = do
    len <- C8.getLine
    strings <- replicateM (toInt len) C8.getLine
    return (map toFact strings)

readMutexGroup :: IO FDR.MutexGroup
readMutexGroup = do
    expect "begin_mutex_group"
    mg <- readFacts
    expect "end_mutex_group"
    return mg

readState :: Int -> IO FDR.State
readState n = do
    expect "begin_state"
    state <- replicateM n C8.getLine
    expect "end_state"
    return $ Vec.fromList (map toInt state)

readGoal :: IO FDR.Goal
readGoal = do
    expect "begin_goal"
    goal <- readFacts
    expect "end_goal"
    return $ Vec.fromList goal

type Effect = (FDR.Fact, FDR.Fact)

-- unsafe
toEffect :: ByteString -> Effect
toEffect s =
    let [_, var, pre, post] = C8.words s in
    ((toInt var, toInt pre), (toInt var, toInt post))

readEffects :: IO [Effect]
readEffects = do
    len <- C8.getLine
    strings <- replicateM (toInt len) C8.getLine
    return (map toEffect strings)

readAction :: IO FDR.Action
readAction = do
    expect "begin_operator"
    name <- C8.getLine
    pre1 <- readFacts
    effects <- readEffects
    cost <- C8.getLine
    expect "end_operator"
    let pre2 = filter (\p -> snd p /= (-1)) $ map fst effects
    -- sort first extracts all increasing/decreasing parts of the list, and merges them
    let pre = sort $ pre1 ++ pre2
    let post = sort $ map snd effects
    return (FDR.Action name pre post (toInt cost))

readSAS :: IO FDR.FDR
readSAS = do
    _ <- readVersion
    _ <- readMetric
    nVars <- C8.getLine
    vars <- replicateM (toInt nVars) readVariable
    nMutexGroup <- C8.getLine
    mutexGroups <- replicateM (toInt nMutexGroup) readMutexGroup
    state <- readState (toInt nVars)
    goal <- readGoal
    nAction <- C8.getLine
    actions <- replicateM (toInt nAction) readAction
    _ <- C8.getLine
    return (FDR.FDR (Vec.fromList vars) mutexGroups state goal actions)
