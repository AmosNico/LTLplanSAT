module ParseSAS (readSAS, toInt) where

import qualified FDR

import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import qualified Data.ByteString.Char8 as C8
import Data.ByteString (ByteString)
import Control.Monad (unless, replicateM)
import System.IO (withFile, IOMode(ReadMode), Handle)

hGetLn :: Handle -> IO ByteString
hGetLn handle = C8.strip <$> C8.hGetLine handle

toInt :: ByteString -> Int
toInt = fst . fromJust . C8.readInt

expect :: Handle -> String -> IO ()
expect handle s = do
    input <- hGetLn handle
    unless (input == C8.pack s) $ putStrLn $
        "Unexpected input while parsing SAS file: Expected "
        ++ show s ++ ", but found " ++ show input ++ "."

readVersion :: Handle -> IO Int
readVersion handle = do
    expect handle "begin_version"
    n <- hGetLn handle
    expect handle "end_version"
    return (toInt n)

readMetric :: Handle -> IO Int
readMetric handle = do
    expect handle "begin_metric"
    n <- hGetLn handle
    expect handle "end_metric"
    return (toInt n)

readVariable :: Handle -> IO FDR.Variable
readVariable handle = do
    expect handle "begin_variable"
    name <- hGetLn handle
    _ <- hGetLn handle
    len <- toInt <$> hGetLn handle
    vals <- replicateM len (hGetLn handle)
    expect handle "end_variable"
    pure $ FDR.Var name (Vec.fromList vals)

-- unsafe
toFact :: ByteString -> FDR.Fact
toFact s =
    let [var, val] = C8.words s in
    (toInt var, toInt val)

readFacts :: Handle -> IO [FDR.Fact]
readFacts handle = do
    len <- hGetLn handle
    strings <- replicateM (toInt len) (hGetLn handle)
    return (map toFact strings)

readMutexGroup :: Handle -> IO FDR.MutexGroup
readMutexGroup handle = do
    expect handle "begin_mutex_group"
    mg <- readFacts handle
    expect handle "end_mutex_group"
    return mg

readState :: Handle -> Int -> IO FDR.State
readState handle n = do
    expect handle "begin_state"
    state <- replicateM n (hGetLn handle)
    expect handle "end_state"
    return $ map toInt state

readGoal :: Handle -> IO FDR.Goal
readGoal handle = do
    expect handle "begin_goal"
    goal <- readFacts handle
    expect handle "end_goal"
    return goal

type Effect = (FDR.Fact, FDR.Fact)

-- unsafe
toEffect :: ByteString -> Effect
toEffect s =
    let [_, var, pre, post] = C8.words s in
    ((toInt var, toInt pre), (toInt var, toInt post))

readEffects :: Handle -> IO [Effect]
readEffects handle = do
    len <- hGetLn handle
    strings <- replicateM (toInt len) (hGetLn handle)
    return (map toEffect strings)

readAction :: Handle -> IO FDR.Action
readAction handle = do
    expect handle "begin_operator"
    name <- hGetLn handle
    pre1 <- readFacts handle
    effects <- readEffects handle
    cost <- toInt <$> hGetLn handle
    expect handle "end_operator"
    let pre2 = filter (\p -> snd p /= (-1)) $ map fst effects
    let pre = sort $ pre1 ++ pre2
    let post = sort $ map snd effects
    return (FDR.Action name pre post cost)

parseSAS :: Handle -> IO FDR.FDR
parseSAS handle = do
    _ <- readVersion handle
    _ <- readMetric handle
    nVars <- toInt <$> hGetLn handle
    vars <- replicateM nVars (readVariable handle)
    nMutexGroup <- toInt <$> hGetLn handle
    mutexGroups <- replicateM nMutexGroup (readMutexGroup handle)
    state <- readState handle nVars
    goal <- readGoal handle
    nAction <- toInt <$> hGetLn handle
    actions <- replicateM nAction (readAction handle)
    _ <- hGetLn handle
    return (FDR.FDR (Vec.fromList vars) mutexGroups state goal actions)

readSAS :: IO FDR.FDR
readSAS = do 
    withFile "output.sas" ReadMode parseSAS
    