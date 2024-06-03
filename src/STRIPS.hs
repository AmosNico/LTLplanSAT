module STRIPS (Goal, Fact, State, Action(..), STRIPS(..), fromFDR) where

import qualified FDR
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import Data.Set (Set)
import qualified Data.Set as Set

type Goal = Set Int
type Fact = Int
type State = Set Int
data Action = Action
    { getPre :: Set Fact
    , getPost :: Set Fact
    , getCost :: Int
    } deriving Show
data STRIPS = STRIPS
    { maxIdx :: Int
    , convertFact :: FDR.Fact -> Fact
    , getActions :: [Action]
    }

convertAction :: (FDR.Fact -> Fact) -> FDR.Action -> Action
convertAction factToInt (FDR.Action _ pre post c) = Action (toSet pre) (toSet post) c where
    toSet = Set.fromAscList . Vec.toList . Vec.map factToInt

fromFDR :: FDR.FDR -> STRIPS
fromFDR pt = STRIPS nfacts factToInt actions where
    f (var, idx) = (idx, (var + 1, idx + length (FDR.getVals $ FDR.getVars pt Vec.! var)))
    nVars = length $ FDR.getVars pt
    list = Vec.unfoldrExactN nVars f (0,0)
    factToInt (var, val) = (list Vec.! var) + val
    nfacts = factToInt (nVars - 1, length (FDR.getVals $ FDR.getVars pt Vec.!  (nVars - 1)) - 1) + 1
    actions = map (convertAction factToInt) $ FDR.getActions pt