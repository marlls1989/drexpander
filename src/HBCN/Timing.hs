{-# LANGUAGE FlexibleContexts #-}
module HBCN.Timing
  (LPVar (..)
  ,TimingLP
  ,constraintCycleTime) where

import           Algebra.Graph.Labelled
import           Control.Monad
import           Data.LinearProgram     hiding ((/))
import           HBCN.Internal
import           Prelude                hiding (Num (..))

data LPVar = Arrival Transition
           | FwDelay String String
           | BwDelay String String
           | FwSlack String String
           | BwSlack String String
           | PseudoClock
           deriving (Show, Read, Eq, Ord)

type TimingLP = LP LPVar Double

arrivalTimeEq cycleTime (place, src, dst) = do
  let src' = Arrival src
  let dst' = Arrival dst
  let delay = case (src, dst) of
        (DataTrans s, DataTrans d) -> FwDelay s d
        (NullTrans s, NullTrans d) -> FwDelay s d
        (DataTrans s, NullTrans d) -> BwDelay s d
        (NullTrans s, DataTrans d) -> BwDelay s d
  setVarBounds delay $ LBound 0
  setVarBounds src' $ LBound 0
  setVarBounds dst' $ LBound 0
  case place of
      Place relaxed fixedDelay token
        | relaxed -> do
          let ct = if token then cycleTime else 0
          linCombination [(1, src'), (-1, dst'), (1, delay)] `equalTo` ct
          maybe
            (linCombination [(1, delay)] `geq` linCombination [(1, PseudoClock)])
            (\x -> linCombination [(1, delay)] `geqTo` x) fixedDelay
        | otherwise -> do
          let ct = if token then cycleTime else 0
          linCombination [(1, src'), (-1, dst'), (1, delay)] `leqTo` ct
          maybe
            (linCombination [(1, delay)] `equal` linCombination [(1, PseudoClock)])
            (\x -> linCombination [(1, delay)] `equalTo` x) fixedDelay
      Unconnected -> error "misformed HBCN"

constraintCycleTime :: HBCN -> Double -> Double -> TimingLP
constraintCycleTime hbcn minDelay cycleTime = execLPM $ do
  setDirection Max
  setObjective $ linCombination [(1, PseudoClock)]
  setVarBounds PseudoClock $ Bound minDelay cycleTime
  mapM_ (arrivalTimeEq cycleTime) $ edgeList hbcn
