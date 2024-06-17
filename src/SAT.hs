{-# LANGUAGE TupleSections #-}

module SAT (solve, Plan) where

import Data.Bits (bit, (.&.))
import Data.List ((\\))
import Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Ersatz as E
import Types
import Constraints (Constraints (constraintsToSat))

numberAtMostOneV :: PlanningTask c -> Int
numberAtMostOneV pt = ceiling (logBase 2 $ fromIntegral $ ptNumberActions pt :: Double)

actionToInt :: PlanningTask c -> Action -> Int
actionToInt pt a = Map.fromList (zip (ptActions pt) [0 ..]) Map.! a

defineVariables :: (E.MonadSAT s m) => PlanningTask c -> Time -> m (Map Variable E.Bit)
defineVariables pt k = sequence $ Map.fromList list
  where
    list = map (,E.exists) (actionVars ++ factVars ++ amoVars)
    actionVars = [ActionV t a | a <- ptActions pt, t <- [1 .. k]]
    factVars = [FactV t f | f <- ptFacts pt, t <- [0 .. k]]
    amoVars = [AtMostOneV t i | i <- [0 .. numberAtMostOneV pt], t <- [1 .. k]]

initialToSAT :: PlanningTask c -> Map Variable E.Bit -> E.Bit
initialToSAT pt v = E.and $ pos ++ neg
  where
    pos = map (\f -> v ! FactV 0 f) $ ptInitalState pt
    neg = map (\f -> E.not $ v ! FactV 0 f) $ ptFacts pt \\ ptInitalState pt

goalToSAT :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
goalToSAT pt k v = E.and $ map (\f -> v ! FactV k f) $ ptGoal pt

actionToSAT :: Action -> Time -> Map Variable E.Bit -> E.Bit
actionToSAT a t v = E.and $ pre ++ add ++ del
  where
    pre = map (\f -> v ! ActionV t a E.==> v ! FactV (t - 1) f) $ actionPre a
    add = map (\f -> v ! ActionV t a E.==> v ! FactV t f) $ actionAdd a
    del = map (\f -> v ! ActionV t a E.==> E.not (v ! FactV t f)) $ actionDel a

mutexToSAT :: MutexGroup -> Time -> Map Variable E.Bit -> E.Bit
mutexToSAT mg t v = E.or $ map (\f -> v ! FactV t f) mg

frameAxioms :: PlanningTask c -> Fact -> Time -> Map Variable E.Bit -> E.Bit
frameAxioms pt f t v = frame1 E.&& frame2
  where
    fInAdd = filter (\a -> f `elem` actionAdd a) $ ptActions pt
    frame1 = E.or $ [v ! FactV (t - 1) f, E.not $ v ! FactV t f] ++ map (\f' -> v ! ActionV t f') fInAdd
    fInDel = filter (\a -> f `elem` actionDel a) $ ptActions pt
    frame2 = E.or $ [E.not $ v ! FactV (t - 1) f, v ! FactV t f] ++ map (\f' -> v ! ActionV t f') fInDel

-- Is the next one better?
atMostOne' :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
atMostOne' pt t v = E.and [constr a i | a <- ptActions pt, i <- [0 .. numberAtMostOneV pt - 1]]
  where
    n a i = (if actionToInt pt a .&. bit i == bit i then id else E.not) $ v ! AtMostOneV t i
    constr a i = v ! ActionV t a E.==> n a i

-- Use the variables "AtMostOneV t i" for i = 0..n to
atMostOne :: PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
atMostOne pt t v = E.and [constr a | a <- ptActions pt]
  where
    constr a = v ! ActionV t a E.==> (bits E.=== encodeAction a)
    bits = E.Bits [v ! AtMostOneV t i | i <- [0 .. numberAtMostOneV pt - 1]]
    encodeAction a = E.encode (toInteger $ actionToInt pt a)

-- k is the maximum number of timesteps in the SAT encoding
ptToSAT :: Constraints c => PlanningTask c -> Time -> Map Variable E.Bit -> E.Bit
ptToSAT pt k v =
  E.and
    [ initialToSAT pt v,
      goalToSAT pt k v,
      E.and [actionToSAT a t v | a <- ptActions pt, t <- [1 .. k]],
      E.and [frameAxioms pt f t v | f <- ptFacts pt, t <- [1 .. k]],
      constraintsToSat (ptConstraints pt) k v,
      E.and [mutexToSAT mg t v | mg <- ptMutexGroups pt, t <- [0 .. k]],
      E.and [atMostOne pt t v | t <- [1 .. k]]
    ]

extractPlan :: PlanningTask c -> Time -> Map Variable Bool -> Plan
extractPlan pt k v = Plan $ mapMaybe extractAction [1 .. k]
  where
    extractAction :: Time -> Maybe Action
    extractAction t = case filter (\a -> v ! ActionV t a) $ ptActions pt of
      [] -> Nothing
      [a] -> Just a
      l ->
        error $
          "The solution contains "
            ++ show (length l)
            ++ " actions at the same time, "
            ++ "namely "
            ++ show l
            ++ "."

callSAT :: Constraints c => PlanningTask c -> Time -> IO (E.Result, Maybe (Map Variable Bool))
callSAT pt k = E.solveWith E.cryptominisat5 $ do
  vars <- defineVariables pt k
  E.assert $ ptToSAT pt k vars
  return vars

iterativeSolve :: Constraints c => PlanningTask c -> Time -> IO Plan
iterativeSolve pt k = do
  putStrLn $ "Calling the SAT-solver with maximal plan length " ++ show k ++ "."
  (res, mSolution) <- callSAT pt k
  case res of
    E.Unsatisfied -> iterativeSolve pt (ceiling (fromIntegral k * sqrt 2 :: Double))
    E.Unsolved -> error "The SAT-solver could not solve the planning problem."
    E.Satisfied -> case mSolution of
      Nothing -> error "The SAT-solver said the planning problem is solvable, but did not return a solution."
      Just solution -> return $ extractPlan pt k solution

solve :: Constraints c => PlanningTask c -> IO Plan
solve pt = do
  iterativeSolve pt 5