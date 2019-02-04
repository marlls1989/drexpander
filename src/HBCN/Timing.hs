{-# LANGUAGE FlexibleContexts #-}
module HBCN.Timing
  (LPVar (..)
  ,TimingLP (..)
  ,constraintCycleTime
  ,arrivalTimeEq)where

import           Algebra.Graph.Labelled
import           Control.Monad
import           Data.LinearProgram
import           HBCN.Internal
import           Prelude                hiding (Num (..))

data LPVar = Arrival Transition
           | FreeSlack Transition Transition
           | ClkPeriod
           | CycleTime
           deriving (Show, Read, Eq, Ord)

type TimingLP = LP LPVar Double

n *& v = linCombination [(n, v)]

arrivalTimeEq (place, src, dst) = let
  src' = 1 *& Arrival src
  dst' = 1 *& Arrival dst
  slack = 1 *& FreeSlack src dst
  period = 1 *& ClkPeriod
  token = (fromIntegral . tokenCount) place *& CycleTime
  in (src' + period + slack - token) `equal` dst'


constraintCycleTime :: HBCN -> Double -> TimingLP
constraintCycleTime hbcn cycleTime = execLPM $ do
  setDirection Max
  setObjective (1 *& ClkPeriod)
  varEq CycleTime cycleTime
  mapM_ arrivalTimeEq $ edgeList hbcn
