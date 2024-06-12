{-# LANGUAGE ScopedTypeVariables #-}
module SAT (toSAT) where

import STRIPS (STRIPS, Action, Fact)
import qualified STRIPS as S
import Data.List ((\\))
import qualified Data.Map.Strict as Map
import Data.Bits (bit, (.&.))

type Time = Int

data Variable = ActionV Time Action | FactV Time Fact | AtMostOneV Time Int

data Literal = Pos Variable | Neg Variable

newtype Clause = Clause [Literal]

newtype CNF = CNF [Clause]

numberAtMostOneV :: STRIPS -> Int
numberAtMostOneV pt = ceiling (logBase 2 $ fromIntegral $ S.numberActions pt :: Double)

actionToInt :: STRIPS -> Action -> Int
actionToInt pt a = Map.fromList (zip (S.actions pt) [0..]) Map.! a

varToInt :: STRIPS -> Time -> Variable -> Int
varToInt pt k (ActionV t a )   = t + k * actionToInt pt a
varToInt pt k (FactV t f)      = t + k * f + k * S.numberActions pt
varToInt pt k (AtMostOneV t i) = t + t * i + k * S.numberActions pt + (k + 1) * S.numberFacts pt

initialToSAT :: STRIPS -> [Clause]
initialToSAT pt = map (\l -> Clause [l]) $ pos ++ neg where
  pos = map (Pos . FactV 0) $ S.initalState pt
  neg = map (Neg . FactV 0) $ S.facts pt \\ S.initalState pt

goalToSAT :: STRIPS -> Time -> [Clause]
goalToSAT pt k = map (\v -> Clause [Pos $ FactV k v]) $ S.goal pt

actionToSAT :: Action -> Time -> [Clause]
actionToSAT a t = pre ++ add ++ del where
  pre = map (\f -> Clause [Neg $ ActionV t a, Pos $ FactV (t-1) f]) $ S.actionPre a
  add = map (\f -> Clause [Neg $ ActionV t a, Pos $ FactV t f]) $ S.actionAdd a
  del = map (\f -> Clause [Neg $ ActionV t a, Neg $ FactV t f]) $ S.actionDel a

frameAxioms :: STRIPS -> Fact -> Time -> [Clause]
frameAxioms pt f t = [frame1, frame2] where
  fInAdd = filter (\a -> f `elem` S.actionAdd a) $ S.actions pt
  frame1 = Clause $ [Pos $ FactV (t-1) f, Neg $ FactV t f] ++ map (Pos . ActionV t) fInAdd
  fInDel = filter (\a -> f `elem` S.actionDel a) $ S.actions pt
  frame2 = Clause $ [Neg $ FactV (t-1) f, Pos $ FactV t f] ++ map (Pos . ActionV t) fInDel

atMostOne :: STRIPS -> Time -> [Clause]
atMostOne pt t = [clause a i | a <- S.actions pt, i <- [0.. numberAtMostOneV pt]] where
  n a i = (if 1 == actionToInt pt a .&. bit i then Pos else Neg) $ AtMostOneV t i
  clause a i = Clause [Neg $ ActionV t a, n a i]

-- k is the maximum number of timesteps in the SAT encoding
toSAT :: STRIPS -> Time -> CNF
toSAT pt k = CNF $
  initialToSAT pt ++
  goalToSAT pt k ++
  concat [actionToSAT a t | a <- S.actions pt, t <- [1..k]] ++
  concat [frameAxioms pt f t | f <- S.facts pt, t <- [1..k]] ++
  concat [atMostOne pt t | t <- [1..k]]
