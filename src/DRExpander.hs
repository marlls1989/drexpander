{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
module DRExpander where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Language.Verilog       as Verilog

data PrgOptions = PrgOptions
  { verilogFiles :: [FilePath]
  , resetName    :: String
  , clkName      :: String
  } deriving (Show)

readVerilogFile :: (MonadIO m) => FilePath -> m [Verilog.Module]
readVerilogFile path = do
  s <- liftIO $ readFile path
  return $ Verilog.parseFile [] path s

readVerilogFiles :: (MonadIO m) => [FilePath] -> m [Verilog.Module]
readVerilogFiles = fmap concat . mapM readVerilogFile

vlogModuleInputs :: Verilog.Module -> Set Verilog.Identifier
vlogModuleInputs (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Input _ xs) = xs
    go _                    = []

vlogModuleOutputs :: Verilog.Module -> Set Verilog.Identifier
vlogModuleOutputs (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Output _ xs) = xs
    go _                     = []

vlogModuleWires :: Verilog.Module -> Set Verilog.Identifier
vlogModuleWires (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Wire _ xs) = map fst xs
    go _                   = []

vlogModuleIntWires :: Verilog.Module -> Set Verilog.Identifier
vlogModuleIntWires mod = wires Set.\\ Set.union inputs outputs
  where
    wires = vlogModuleWires mod
    inputs = vlogModuleInputs mod
    outputs = vlogModuleOutputs mod

vlogModuleWithoutWires :: Verilog.Module -> Verilog.Module
vlogModuleWithoutWires (Verilog.Module name _ items) =
  Verilog.Module name [] $ filter p items where
  p (Verilog.Input _ _)  = False
  p (Verilog.Output _ _) = False
  p (Verilog.Wire _ _)   = False
  p _                    = True

vlogDRWireInstance :: Verilog.Identifier -> Verilog.ModuleItem
vlogDRWireInstance name = Verilog.Instance "drwire" [] name []

vlogDRWireInput :: Verilog.Identifier -> Verilog.Identifier
vlogDRWireInput = ("drwire.in " ++)

vlogDRWireOutput :: Verilog.Identifier -> Verilog.Identifier
vlogDRWireOutput = ("drwire.out " ++)

vlogWireInput :: Verilog.Identifier -> Verilog.Identifier
vlogWireInput = ("input " ++)

fixDffReset :: (MonadReader PrgOptions m) => Verilog.ModuleItem -> m Verilog.ModuleItem
fixDffReset inst@(Verilog.Instance mname parms name portmap)
  | mname == "dff" || mname == "dffen" = do
      env <- ask
      let resetPin = (Just "reset", Just . Verilog.Ident $ resetName env)
      return $ Verilog.Instance mname parms name (resetPin : portmap)
  | otherwise = return inst
fixDffReset x = return x

processModule :: (MonadReader PrgOptions m) => Verilog.Module -> m Verilog.Module
processModule mod = do
  options <- ask
  let clkAndReset = [clkName options, resetName options]
  let inputs = vlogModuleInputs mod Set.\\ clkAndReset
  let outputs = vlogModuleOutputs mod
  let wires = vlogModuleIntWires mod
  let (Verilog.Module mname _ mitems) = vlogModuleWithoutWires mod
  let drInputs = vlogDRWireInput <$> Set.elems inputs
  let drOutputs = vlogDRWireOutput <$> Set.elems outputs
  let drWires = vlogDRWireInstance <$> Set.elems wires
  let wireClkRst = vlogWireInput <$> Set.elems clkAndReset
  let margs = drInputs ++ drOutputs ++ wireClkRst
  instances <- mapM fixDffReset mitems
  return $ Verilog.Module mname margs (drWires ++ instances)

processVerilogFile :: (MonadReader PrgOptions m, MonadIO m) => FilePath -> m [Verilog.Module]
processVerilogFile path = readVerilogFile path >>= mapM processModule

processVerilogFiles :: (MonadReader PrgOptions m, MonadIO m) => [FilePath] -> m [Verilog.Module]
processVerilogFiles = fmap concat . mapM processVerilogFile
