module STRIPS (Goal, Fact, State, Action(..), STRIPS(..), fromFDR, printSTRIPS) where

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
    }
data STRIPS = STRIPS
    { numberFacts :: Int
    , showFact :: Fact -> ByteString
    , actions :: [Action]
    , mutexGroups :: [MutexGroup]
    }

showFacts :: STRIPS -> [Fact] -> ByteString
showFacts pt facts = C8.intercalate (C8.pack ", ") $ map (showFact pt) facts

showAction :: STRIPS -> Action -> ByteString
showAction pt (Action name pre add del cost) = C8.concat
    [name, C8.pack " (", C8.pack (show cost), 
     C8.pack "): pre = {", showFacts pt pre, 
     C8.pack "}, post = {", showFacts pt add,
     C8.pack "}, del = {",  showFacts pt del, C8.pack "}"]
         
printSTRIPS :: STRIPS -> IO ()
printSTRIPS pt = C8.putStrLn $ C8.concat
    [C8.pack "There are ", C8.pack (show (numberFacts pt)), C8.pack " facts: ",
     showFacts pt [0..numberFacts pt - 1],
     C8.pack "\nActions:\n", C8.unlines (map (showAction pt) (actions pt)),
     C8.pack "Mutex Groups:\n", C8.unlines (map (showFacts pt) (mutexGroups pt))]     

-- Compute the hash of v = 0 for all FDR variables v. The last element is the number of facts
perfectHash :: FDR.FDR -> Vector Int
perfectHash fdr = Vec.unfoldrExactN (FDR.nVars fdr + 1) f (0,0) where
    -- f iterates through all variables v and computes the hash for v = 0, 
    -- while remembering number of the variable and hash value
    f (var, hash) = (hash, (var + 1, hash + FDR.domainSize fdr var))

convertFact :: FDR.FDR -> FDR.Fact -> Fact
convertFact pt (var, val) = (perfectHash pt Vec.! var) + val

inverseHash :: FDR.FDR -> Int -> Vector FDR.Fact
inverseHash fdr nFacts = Vec.unfoldrExactN nFacts f (0,0) where
    f (var, val) = 
        if val + 1 == FDR.domainSize fdr var
        then ((var,val), (var + 1, 0))
        else ((var,val), (var, val + 1))

deletingEffects :: FDR.FDR -> FDR.Action -> [Fact]
deletingEffects pt a = concatMap f (FDR.actionPost a) where 
    getPre var = find (\(var', _) -> var == var') (FDR.actionPre a)
    f (var, val) = case getPre var of
        Just (_, val') -> [convertFact pt (var, val')]
        Nothing -> [convertFact pt (var,val') | val' <- [0.. FDR.domainSize pt var - 1], val /= val']

convertAction :: FDR.FDR -> FDR.Action -> Action
convertAction pt a@(FDR.Action name pre post c) = Action name pre' add del c where
    pre' = map (convertFact pt) pre
    add = map (convertFact pt) post
    del = deletingEffects pt a

fromFDR :: FDR.FDR -> STRIPS
fromFDR fdr = STRIPS nfacts showfact as (oldMGs ++ newMGs) where
    nfacts = perfectHash fdr Vec.! FDR.nVars fdr
    showfact fact = FDR.showFact fdr $ inverseHash fdr nfacts Vec.! fact
    -- showFact fact = C8.pack $ show $ inverseHash fdr nfacts Vec.! fact
    as = map (convertAction fdr) $ FDR.actions fdr
    oldMGs = map (map $ convertFact fdr) (FDR.mutexGroups fdr)
    newMGs = [[convertFact fdr (var, val) | val <- [0..FDR.domainSize fdr var - 1]] 
             | var <- [0..FDR.nVars fdr -1]]