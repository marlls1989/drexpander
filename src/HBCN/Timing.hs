{-# LANGUAGE FlexibleContexts #-}
module HBCN.Timing
  (LPVar (..)
  ,TimingLP (..)
  ,constraintCycleTime
  ,arrivalTimeEq)where

import           Algebra.Graph.Labelled
import           Control.Monad
import           Data.LinearProgram
import           Data.LinearProgram.LinExpr
import           HBCN.Internal
import           Prelude                    hiding (Num (..))

data LPVar = Arrival Transition
           | FreeSlack Transition Transition
           | ClkPeriod
           deriving (Show, Read, Eq, Ord)

type TimingLP = LP LPVar Double

arrivalTimeEq cycleTime (place, src, dst) = do
  let src' = Arrival src
  let dst' = Arrival dst
  let slack = FreeSlack src dst
  let tokens = if place == Token then cycleTime else 0
  linCombination [(1, src'), (1, ClkPeriod), (-1, dst'), (1, slack)]  `equalTo` tokens
  setVarBounds slack $ LBound 0

constraintCycleTime :: HBCN -> Double -> TimingLP
constraintCycleTime hbcn cycleTime = execLPM $ do
  setDirection Max
  setObjective (linCombination [(1, ClkPeriod)])
  setVarBounds ClkPeriod $ LBound 0
  mapM_ (arrivalTimeEq cycleTime) $ edgeList hbcn
