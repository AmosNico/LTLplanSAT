module ParseSAS (readSAS) where

import Control.Exception (Exception, throw)
import Control.Monad (replicateM, unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.List (sort)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import SAS (SAS)
import qualified SAS
import System.IO (Handle, IOMode (ReadMode), withFile)

data ParseError = ParseError String String

instance Show ParseError where
  show (ParseError expected found) =
    "Unexpected input while parsing SAS-file: Expected "
      ++ expected
      ++ ", but got "
      ++ found
      ++ "."

instance Exception ParseError

hGetLn :: Handle -> IO ByteString
hGetLn handle = C8.strip <$> C8.hGetLine handle

toInt :: ByteString -> Int
toInt = fst . fromJust . C8.readInt

expect :: Handle -> String -> IO ()
expect handle s = do
  input <- hGetLn handle
  unless (input == C8.pack s) $ throw $ ParseError (show s) (show input)

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

readVariable :: Handle -> IO SAS.Variable
readVariable handle = do
  expect handle "begin_variable"
  name <- hGetLn handle
  _ <- hGetLn handle
  len <- toInt <$> hGetLn handle
  vals <- replicateM len (hGetLn handle)
  expect handle "end_variable"
  pure $ SAS.Var name (Vec.fromList vals)

-- unsafe
toFact :: ByteString -> SAS.Fact
toFact s = case C8.words s of
  [var, val] -> (toInt var, toInt val)
  _ ->
    throw $
      ParseError
        "a fact \"var val\" where var and val are integers"
        (show s)

readFacts :: Handle -> IO [SAS.Fact]
readFacts handle = do
  len <- hGetLn handle
  strings <- replicateM (toInt len) (hGetLn handle)
  return (map toFact strings)

readMutexGroup :: Handle -> IO SAS.MutexGroup
readMutexGroup handle = do
  expect handle "begin_mutex_group"
  mg <- readFacts handle
  expect handle "end_mutex_group"
  return mg

readState :: Handle -> Int -> IO SAS.State
readState handle n = do
  expect handle "begin_state"
  state <- replicateM n (hGetLn handle)
  expect handle "end_state"
  return $ map toInt state

readGoal :: Handle -> IO SAS.Goal
readGoal handle = do
  expect handle "begin_goal"
  goal <- readFacts handle
  expect handle "end_goal"
  return goal

type Effect = (SAS.Fact, SAS.Fact)

-- unsafe
toEffect :: ByteString -> Effect
toEffect s = case C8.words s of
  [_, var, pre, post] -> ((toInt var, toInt pre), (toInt var, toInt post))
  _ ->
    throw $
      ParseError
        "an effect \"_ var pre post\" where var, pre and post are integers"
        (show s)

readEffects :: Handle -> IO [Effect]
readEffects handle = do
  len <- hGetLn handle
  strings <- replicateM (toInt len) (hGetLn handle)
  return (map toEffect strings)

readAction :: Handle -> IO SAS.Action
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
  return (SAS.Action name pre post cost)

parseSAS :: Handle -> IO SAS
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
  return (SAS.SAS (Vec.fromList vars) mutexGroups state goal actions)

readSAS :: FilePath -> IO SAS
readSAS path = withFile path ReadMode parseSAS