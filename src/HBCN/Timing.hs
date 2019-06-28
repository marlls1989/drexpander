{-# LANGUAGE FlexibleContexts #-}
module HBCN.Timing
  (LPVar (..)
  ,TimingLP (..)
  ,constraintCycleTime
  ,arrivalTimeEq
  ,arrivalTimeConstraint
  ,computePseudoClock) where

import           Algebra.Graph.Labelled
import           Control.Monad
import           Data.LinearProgram
import           Data.LinearProgram.LinExpr
import           HBCN.Internal
import           Prelude                    hiding (Num (..))

data LPVar = Arrival Transition
           | FwDelay String String
           | BwDelay String String
           | FwSlack String String
           | BwSlack String String
           | PseudoClock
           | DelayFactor
           deriving (Show, Read, Eq, Ord)

type TimingLP = LP LPVar Double

arrivalTimeConstraint cycleTime minDelay (place, src, dst) = do
  let src' = Arrival src
  let dst' = Arrival dst
  let ct = if hasToken place then cycleTime else 0
  let slack = case (src, dst) of
        (DataTrans s, DataTrans d) -> FwSlack s d
        (NullTrans s, NullTrans d) -> FwSlack s d
        (DataTrans s, NullTrans d) -> BwSlack s d
        (NullTrans s, DataTrans d) -> BwSlack s d
  let delay = case (src, dst) of
        (DataTrans s, DataTrans d) -> FwDelay s d
        (NullTrans s, NullTrans d) -> FwDelay s d
        (DataTrans s, NullTrans d) -> BwDelay s d
        (NullTrans s, DataTrans d) -> BwDelay s d
  setVarBounds delay $ LBound minDelay
  setVarBounds slack $ LBound 0
  setVarBounds src' $ LBound 0
  setVarBounds dst' $ LBound 0
  linCombination [(1, src'), (-1, dst'), (1, delay)] `equalTo` ct
  linCombination [(1, delay)] `equal` linCombination [(1, PseudoClock), (1, slack)]

arrivalTimeEq cycleTime minDelay biasing (place, src, dst) = do
  let src' = Arrival src
  let dst' = Arrival dst
  let ct = if hasToken place then cycleTime else 0
  let delay = case (src, dst) of
        (DataTrans s, DataTrans d) -> FwDelay s d
        (NullTrans s, NullTrans d) -> FwDelay s d
        (DataTrans s, NullTrans d) -> BwDelay s d
        (NullTrans s, DataTrans d) -> BwDelay s d
  let slack = case (src, dst) of
        (DataTrans s, DataTrans d) -> FwSlack s d
        (NullTrans s, NullTrans d) -> FwSlack s d
        (DataTrans s, NullTrans d) -> BwSlack s d
        (NullTrans s, DataTrans d) -> BwSlack s d
  setVarBounds delay $ LBound minDelay
  setVarBounds slack $ LBound 0
  setVarBounds src' $ LBound 0
  setVarBounds dst' $ LBound 0
  linCombination [(1, src'), (-1, dst'), (1, delay)]  `equalTo` ct
  linCombination [(1, delay)] `equal` linCombination [(biasing + weight place, DelayFactor), (1, slack)]
  linCombination [(1, PseudoClock)] `leq` linCombination [(1, delay)]

constraintCycleTime :: HBCN -> Double -> Double -> Double -> TimingLP
constraintCycleTime hbcn cycleTime minDelay biasing = execLPM $ do
  setDirection Max
  setObjective (linCombination [(1, DelayFactor)])
  setVarBounds DelayFactor $ LBound 0
  setVarBounds PseudoClock $ LBound minDelay
  mapM_ (arrivalTimeEq cycleTime minDelay biasing) $ edgeList hbcn

computePseudoClock :: HBCN -> Double -> Double -> TimingLP
computePseudoClock hbcn cycleTime minDelay = execLPM $ do
  setDirection Max
  setObjective $ linCombination [(1, PseudoClock)]
  setVarBounds PseudoClock $ LBound minDelay
  mapM_ (arrivalTimeConstraint cycleTime minDelay) $ edgeList hbcn
