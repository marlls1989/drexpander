{-# LANGUAGE FlexibleContexts #-}
module HBCN.Timing
  (LPVar (..)
  ,TimingLP
  ,constraintCycleTime) where

import           Algebra.Graph.Labelled
import           Control.Monad
import           Data.LinearProgram
import           HBCN.Internal
import           Prelude                    hiding (Num (..))

data LPVar = Arrival Transition
           | FwDelay String String
           | BwDelay String String
           | FwSlack String String
           | BwSlack String String
           | PseudoClock
           deriving (Show, Read, Eq, Ord)

type TimingLP = LP LPVar Double

arrivalTimeEq cycleTime minDelay relax (place, src, dst) = do
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
  if relax
    then do
    linCombination [(1, src'), (-1, dst'), (1, delay)] `equalTo` ct
    case place of
      MindelayPlace _ -> linCombination [(1, delay), (-1, slack)] `equalTo` minDelay
      _ -> linCombination [(1, delay)] `equal` linCombination [(1, PseudoClock), (1, slack)]
    else do
    linCombination [(1, src'), (-1, dst'), (1, delay), (1, slack)] `equalTo` ct
    case place of
      MindelayPlace _ -> linCombination [(1, delay)] `equalTo` minDelay
      _ -> linCombination [(1, delay)] `equal` linCombination [(1, PseudoClock)]

constraintCycleTime :: HBCN -> Double -> Double -> Bool -> TimingLP
constraintCycleTime hbcn cycleTime minDelay relax = execLPM $ do
  setDirection Max
  setObjective $ linCombination [(1, PseudoClock)]
  setVarBounds PseudoClock $ LBound minDelay
  mapM_ (arrivalTimeEq cycleTime minDelay relax) $ edgeList hbcn
