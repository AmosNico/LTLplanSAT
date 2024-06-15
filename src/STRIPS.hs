module STRIPS (Fact, MutexGroup, State, Goal, Action(..), STRIPS(..), 
               printSTRIPS, fromFDR, facts, numberActions) where

import qualified FDR
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.List (find)

type Fact = Int
type MutexGroup = [Fact]
type State = [Fact]
type Goal = [Fact]
data Action = Action
    { actionName :: ByteString
    , actionPre :: [Fact]
    , actionAdd :: [Fact]
    , actionDel :: [Fact]
    , actionCost :: Int
    } deriving(Eq, Ord)
data STRIPS = STRIPS
    { numberFacts :: Int
    , showFact :: Fact -> ByteString
    , actions :: [Action]
    , initalState :: State
    , goal :: Goal
    , mutexGroups :: [MutexGroup]
    }

instance Show Action where
    show a = C8.unpack $ actionName a

facts :: STRIPS -> [Fact]
facts pt = [0 .. numberFacts pt - 1]

numberActions :: STRIPS -> Int
numberActions pt = length $ actions pt

showFacts :: STRIPS -> [Fact] -> ByteString
showFacts pt fs = C8.intercalate (C8.pack ", ") $ map (showFact pt) fs

showAction :: STRIPS -> Action -> ByteString
showAction pt (Action name pre add del cost) = C8.concat
    [name, C8.pack " (cost ", C8.pack (show cost), 
     C8.pack "): \n  pre = {", showFacts pt pre, 
     C8.pack "}, \n  post = {", showFacts pt add,
     C8.pack "}, \n  del = {",  showFacts pt del, C8.pack "}"]
         
printSTRIPS :: STRIPS -> IO ()
printSTRIPS pt = C8.putStrLn $ C8.concat
    [C8.pack "There are ", C8.pack (show (numberFacts pt)), C8.pack " facts:\n",
     showFacts pt [0..numberFacts pt - 1],
     C8.pack "\nActions:\n", C8.unlines (map (showAction pt) (actions pt)),
     C8.pack "Initial state:\n", C8.unlines $ map (showFact pt) (initalState pt),
     C8.pack "Goal:\n", showFacts pt (goal pt),
     C8.pack "\nMutex Groups:\n", C8.unlines (map (showFacts pt) (mutexGroups pt))]     

-- Compute the hash of v = 0 for all FDR variables v. The last element is the number of facts
perfectHash :: FDR.FDR -> Vector Int
perfectHash fdr = Vec.unfoldrExactN (FDR.nVars fdr + 1) f (0,0) where
    -- f iterates through all variables v and computes the hash for v = 0, 
    -- while remembering number of the variable and hash value
    f (var, hash) = (hash, (var + 1, hash + FDR.domainSize fdr var))

convertFact :: FDR.FDR -> FDR.Fact -> Fact
convertFact pt (var, val) = (perfectHash pt Vec.! var) + val

convertFacts :: FDR.FDR -> [FDR.Fact] -> [Fact]
convertFacts pt = map (convertFact pt)

inverseHash :: FDR.FDR -> Int -> Vector FDR.Fact
inverseHash fdr nFacts = Vec.unfoldrExactN nFacts f (0,0) where
    f (var, val) = 
        if val + 1 == FDR.domainSize fdr var
        then ((var,val), (var + 1, 0))
        else ((var,val), (var, val + 1))

deletingEffects :: FDR.FDR -> FDR.Action -> [Fact]
deletingEffects pt a = concatMap f (FDR.actionPost a) where 
    getPre :: Int -> Maybe FDR.Fact
    getPre var = find (\(var', _) -> var == var') (FDR.actionPre a)
    -- get the deleting effects for the variable var
    f (var, val) = case getPre var of
        Just (_, val') -> [convertFact pt (var, val')]
        Nothing -> [convertFact pt (var,val') | val' <- [0.. FDR.domainSize pt var - 1], val /= val']

convertAction :: FDR.FDR -> FDR.Action -> Action
convertAction pt a@(FDR.Action name pre post c) = Action name pre' add del c where
    pre' = convertFacts pt pre
    add = convertFacts pt post
    del = deletingEffects pt a

fromFDR :: FDR.FDR -> STRIPS
fromFDR fdr@(FDR.FDR _ mgs s g as) = STRIPS nfacts showfact as' s' g' (oldMGs ++ newMGs) where
    nfacts = perfectHash fdr Vec.! FDR.nVars fdr
    showfact fact = FDR.showFact fdr $ inverseHash fdr nfacts Vec.! fact
    as' = map (convertAction fdr) as
    s' = convertFacts fdr (zip [0..] s)
    g' = convertFacts fdr g
    oldMGs = map (convertFacts fdr) mgs
    newMGs = [[convertFact fdr (var, val) | val <- [0..FDR.domainSize fdr var - 1]] 
             | var <- [0..FDR.nVars fdr -1]]