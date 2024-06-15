{-# LANGUAGE TupleSections #-}
module SAT (solve, Plan) where

import STRIPS (STRIPS, Action, Fact)
import qualified STRIPS as S
import Data.List ((\\), intercalate)
import qualified Data.Map.Strict as Map
import Data.Bits (bit, (.&.))
import qualified Ersatz as E
import Data.Map (Map,(!))
import Data.Maybe (mapMaybe)

type Time = Int

data Variable = ActionV Time Action | FactV Time Fact | AtMostOneV Time Int
  deriving (Eq, Ord, Show)

newtype Plan = Plan [Action]

instance Show Plan where
  show (Plan as) = 
    "Plan with length " ++ show (length as) ++ 
    " and cost " ++ show (sum $ map S.actionCost as) ++ ":\n  " 
    ++ intercalate "\n  " (map show as)

numberAtMostOneV :: STRIPS -> Int
numberAtMostOneV pt = ceiling (logBase 2 $ fromIntegral $ S.numberActions pt :: Double)

actionToInt :: STRIPS -> Action -> Int
actionToInt pt a = Map.fromList (zip (S.actions pt) [0..]) Map.! a

initialToSAT :: STRIPS -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ pos ++ neg where
  pos = map (\f -> v ! FactV 0 f) $ S.initalState pt
  neg = map (\f -> E.not $ v ! FactV 0 f) $ S.facts pt \\ S.initalState pt

goalToSAT :: STRIPS -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (\f -> v ! FactV k f) $ S.goal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add ++ del where
  pre = map (\f -> v ! ActionV t a E.==> v ! FactV (t-1) f) $ S.actionPre a
  add = map (\f -> v ! ActionV t a E.==> v ! FactV t f) $ S.actionAdd a
  del = map (\f -> v ! ActionV t a E.==> E.not (v ! FactV t f)) $ S.actionDel a

frameAxioms :: STRIPS -> Fact -> Time -> Map Variable E.Bit -> E.Bit
frameAxioms pt f t v = frame1 E.&& frame2 where
  fInAdd = filter (\a -> f `elem` S.actionAdd a) $ S.actions pt
  frame1 = E.or $ [v ! FactV (t-1) f, E.not $ v ! FactV t f] ++ map (\f' -> v ! ActionV t f') fInAdd
  fInDel = filter (\a -> f `elem` S.actionDel a) $ S.actions pt
  frame2 = E.or $ [E.not $ v ! FactV (t-1) f, v ! FactV t f] ++ map (\f' -> v ! ActionV t f') fInDel

-- Is the next one better?
atMostOne' :: STRIPS -> Time -> Map Variable E.Bit -> E.Bit
atMostOne' pt t v =  E.and [constr a i | a <- S.actions pt, i <- [0.. numberAtMostOneV pt - 1]] where
  n a i = (if actionToInt pt a .&. bit i  == bit i then id else E.not) $ v ! AtMostOneV t i
  constr a i = v ! ActionV t a E.==> n a i

-- Use the variables "AtMostOneV t i" for i = 0..n to
atMostOne :: STRIPS -> Time -> Map Variable E.Bit -> E.Bit
atMostOne pt t v =  E.and [constr a | a <- S.actions pt] where
  constr a = v ! ActionV t a E.==> (bits E.=== encodeAction a)
  bits = E.Bits [v ! AtMostOneV t i | i <- [0.. numberAtMostOneV pt - 1]]
  encodeAction a = E.encode (toInteger $ actionToInt pt a)

defineVariables :: (E.MonadSAT s m) => STRIPS -> Time -> m (Map Variable E.Bit)
defineVariables pt k = sequence $ Map.fromList list where
  list = map (, E.exists) (actionVars ++ factVars ++ amoVars)
  actionVars = [ActionV t a | a <- S.actions pt, t <- [1..k]]
  factVars = [FactV t f | f <- S.facts pt, t <- [0..k]]
  amoVars = [AtMostOneV t i | i <- [0..numberAtMostOneV pt], t <- [1..k]]

{-defineAMOVariables :: (E.MonadSAT s m) => STRIPS -> Time -> m E.Bits
defineAMOVariables pt k = fmap Bits (replicateM n exists) l where
  n = -}
  

-- k is the maximum number of timesteps in the SAT encoding
constraints :: STRIPS -> Time -> Map Variable E.Bit -> E.Bit
constraints pt k v = E.and [
  initialToSAT pt v,
  goalToSAT pt k v,
  E.and [actionToSAT a t v | a <- S.actions pt, t <- [1..k]],
  E.and [frameAxioms pt f t v | f <- S.facts pt, t <- [1..k]],
  E.and [atMostOne pt t v | t <- [1..k]]
  ]

extractPlan :: STRIPS -> Time -> Map Variable Bool -> Plan
extractPlan pt k v = Plan $ mapMaybe extractAction [1..k] where
  extractAction :: Time -> Maybe Action
  extractAction t = case filter (\ a -> v ! ActionV t a) $ S.actions pt of
    [] -> Nothing
    [a] -> Just a
    l -> error $ "The solution contains " ++ show (length l) ++ " actions at the same time, "
      ++ "namely " ++ show l ++ "."

callSAT :: STRIPS -> Time -> IO (E.Result, Maybe (Map Variable Bool))
callSAT pt k = E.solveWith E.cryptominisat5 $ do
  vars <- defineVariables pt k
  E.assert $ constraints pt k vars
  return vars

iterativeSolve :: STRIPS -> Time -> IO Plan
iterativeSolve pt k = do
  putStrLn $ "Calling the SAT-solver with maximal plan length " ++ show k ++ "."
  (res, mSolution) <- callSAT pt k
  case res of
    E.Unsatisfied -> iterativeSolve pt (ceiling (fromIntegral k * sqrt 2 :: Double))
    E.Unsolved -> error "The SAT-solver could not solve the planning problem."
    E.Satisfied -> case mSolution of
      Nothing -> error "The SAT-solver said the planning problem is solvable, but did not return a solution."
      Just solution -> return $ extractPlan pt k solution

solve :: STRIPS -> IO Plan
solve pt = do
  iterativeSolve pt 5