{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
module HBCN.Timing
  (LPVar (..)
  ,TimingLP
  ,constraintCycleTime
  ,constraintReflexivePaths) where

import           Algebra.Graph.Labelled
import           Control.Monad
import           Data.LinearProgram     hiding ((/))
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           HBCN.Internal
import           Prelude                hiding (Num (..))

data LPVar = Arrival Transition
           | FwDelay String String
           | BwDelay String String
           | FwSlack String String
           | BwSlack String String
           | MinDelay String String
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
  let relaxed = relaxedPlaceDelay place
  let token = hasToken place
  let fixedDelay = fixedPlaceDelay place
  let ct = if token then cycleTime else 0
  setVarBounds delay $ LBound 0
  setVarBounds src' $ LBound 0
  setVarBounds dst' $ LBound 0
  if relaxed then do
    linCombination [(1, src'), (-1, dst'), (1, delay)] `equalTo` ct
    maybe
      (linCombination [(1, delay)] `geq` linCombination [(1, PseudoClock)])
      (\x -> linCombination [(1, delay)] `geqTo` x) fixedDelay
  else do
    linCombination [(1, src'), (-1, dst'), (1, delay)] `leqTo` ct
    maybe
      (linCombination [(1, delay)] `equal` linCombination [(1, PseudoClock)])
      (\x -> linCombination [(1, delay)] `equalTo` x) fixedDelay

constraintCycleTime :: HBCN -> Double -> Double -> TimingLP
constraintCycleTime hbcn minDelay cycleTime = execLPM $ do
  setDirection Max
  setObjective $ linCombination [(1, PseudoClock)]
  setVarBounds PseudoClock $ Bound minDelay cycleTime
  mapM_ (arrivalTimeEq cycleTime) $ edgeList hbcn

constraintReflexivePaths :: HBCN -> Map LPVar Double -> TimingLP
constraintReflexivePaths hbcn variables = execLPM $ do
  setDirection Min
  mapM_ (uncurry varEq) $ Map.assocs variables
  mapM_ (\(place, src, dst) -> let
             src' = nodeName src
             dst' = nodeName dst
             mdVar = MinDelay src' dst'
           in case place of
             ReflexivePlace { relatedTransitions } -> do
               addObjective $ linCombination [(1, mdVar)]
               setVarBounds mdVar $ LBound 0
               mapM_ (\related -> linCombination [(1, mdVar)] `geq`
                                  linCombination [(1, FwDelay src' $ nodeName related)]) relatedTransitions
             _ -> return ()
        ) $ edgeList hbcn
