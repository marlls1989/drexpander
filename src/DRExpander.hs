{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}
module DRExpander where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.BitVec
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Language.Verilog       as Verilog
import Data.List

data Wire = Wire String
          | Bus Integer Integer String
          deriving (Eq, Ord)

data PrgOptions = PrgOptions
  { verilogFiles :: [FilePath]
  , resetName    :: String
  , clkName      :: String
  } deriving (Show)

bitBlastWire :: Wire -> [Wire]
bitBlastWire (Bus x y name) = map (Wire . expandBusWireName name) [x'..y'] where
  x' = min x y
  y' = max x y
bitBlastWire x              = [x]

expandBusWireName :: String -> Integer -> String
expandBusWireName name idx = name ++ "_" ++ show idx

readVerilogFile :: (MonadIO m) => FilePath -> m [Verilog.Module]
readVerilogFile path = do
  s <- liftIO $ readFile path
  return $ Verilog.parseFile [] path s

readVerilogFiles :: (MonadIO m) => [FilePath] -> m [Verilog.Module]
readVerilogFiles = fmap concat . mapM readVerilogFile

vlogModuleInputs :: Verilog.Module -> Set Wire
vlogModuleInputs (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Input Nothing xs)       = map Wire xs
    go (Verilog.Input (Just (Verilog.Number x, Verilog.Number y)) xs) = map (Bus x' y') xs where
      y' = value y
      x' = value x
    go _                               = []

vlogModuleOutputs :: Verilog.Module -> Set Wire
vlogModuleOutputs (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Output Nothing xs)       = map Wire xs
    go (Verilog.Output (Just (Verilog.Number x, Verilog.Number y)) xs) = map (Bus x' y') xs where
      y' = value y
      x' = value x
    go _                               = []

vlogModuleWires :: Verilog.Module -> Set Wire
vlogModuleWires (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Wire Nothing xs)       = map (Wire . fst) xs
    go (Verilog.Wire (Just (Verilog.Number x, Verilog.Number y)) xs) = map (Bus x' y') xs' where
      y' = value y
      x' = value x
      xs' = map fst xs
    go _                               = []

vlogModuleIntWires :: Verilog.Module -> Set Wire
vlogModuleIntWires mod = wires Set.\\ Set.union inputs outputs
  where
    wires = vlogModuleWires mod
    inputs = vlogModuleInputs mod
    outputs = vlogModuleOutputs mod

vlogModuleAllWires :: Verilog.Module -> Set Wire
vlogModuleAllWires mod = Set.unions [wires, inputs, outputs]
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

vlogDRAdaptor :: String -> String -> Maybe Integer -> Verilog.ModuleItem
vlogDRAdaptor mname name Nothing = Verilog.Instance mname [] ("i" ++ name)
  [(Just "t", Just . Verilog.Ident $ name ++ "_t"),
   (Just "f", Just . Verilog.Ident $ name ++ "_f"),
   (Just "ack", Just . Verilog.Ident $ name ++ "_ack"),
   (Just "drw", Just . Verilog.Ident $ name)]
vlogDRAdaptor mname name (Just idx) = Verilog.Instance mname [] ("i" ++ name')
  [(Just "t", Just . Verilog.IdentBit (name ++ "_t") . Verilog.Number $ fromInteger idx),
   (Just "f", Just . Verilog.IdentBit (name ++ "_f") . Verilog.Number $ fromInteger idx),
   (Just "ack", Just . Verilog.IdentBit (name ++ "_ack") . Verilog.Number $ fromInteger idx),
   (Just "drw", Just $ Verilog.Ident name')]
  where
    name' = expandBusWireName name idx

vlogDRWireInstance :: Wire -> [Verilog.ModuleItem]
vlogDRWireInstance (Wire name) = [Verilog.Instance "drwire" [] name []]
vlogDRWireInstance bus = concatMap vlogDRWireInstance $ bitBlastWire bus

vlogDRWirePort :: Verilog.Identifier -> [Verilog.Identifier]
vlogDRWirePort name = map (name ++) ["_t", "_f", "_ack"]

vlogDRWireInputInst :: Wire -> [Verilog.ModuleItem]
vlogDRWireInputInst (Wire name) =
    [Verilog.Input Nothing $ map (name ++) ["_t", "_f"]
    ,Verilog.Output Nothing [name ++ "_ack"]
    ,vlogDRAdaptor "drinput" name Nothing]
vlogDRWireInputInst (Bus x y name) =
    [Verilog.Input range $ map (name ++) ["_t", "_f"]
    ,Verilog.Output range [name ++ "_ack"]] ++
    concatMap go values
  where
    range = Just (Verilog.Number $ fromInteger x, Verilog.Number $ fromInteger y)
    values = [x'..y'] :: [Integer]
    x' = min x y
    y' = max x y
    go :: Integer -> [Verilog.ModuleItem]
    go i = [vlogDRAdaptor "drinput" name $ Just i]


vlogDRWireOutputInst :: Wire -> [Verilog.ModuleItem]
vlogDRWireOutputInst (Wire name) =
  [Verilog.Output Nothing $ map (name ++) ["_t", "_f"]
  ,Verilog.Input Nothing [name ++ "_ack"]
  ,vlogDRAdaptor "droutput" name Nothing]
vlogDRWireOutputInst (Bus x y name) =
    [Verilog.Output range $ map (name ++) ["_t", "_f"]
    ,Verilog.Input range [name ++ "_ack"]] ++
    concatMap go values
  where
    range = Just (Verilog.Number $ fromInteger x, Verilog.Number $ fromInteger y)
    values = [x'..y'] :: [Integer]
    x' = min x y
    y' = max x y
    go :: Integer -> [Verilog.ModuleItem]
    go i = [vlogDRAdaptor "droutput" name $ Just i]

dedupList :: (Ord a) => [a] -> [a]
dedupList = map head . group . sort

fixDffReset :: (MonadReader PrgOptions m) => Verilog.ModuleItem -> m Verilog.ModuleItem
fixDffReset inst@(Verilog.Instance mname parms name portmap)
  | mname == "dff" = do
      env <- ask
      let resetPin = (Just "reset", Just . Verilog.Ident $ resetName env)
      return $ Verilog.Instance mname parms name (resetPin : portmap)
  | otherwise = return inst
fixDffReset x = return x

fixTieResetClk :: (MonadReader PrgOptions m) => Verilog.ModuleItem -> m Verilog.ModuleItem
fixTieResetClk inst@(Verilog.Instance mname parms name portmap)
  | mname == "tielo" || mname == "tiehi" = do
      env <- ask
      let resetPin = (Just "reset", Just . Verilog.Ident $ resetName env)
      let clkPin = (Just "ck", Just . Verilog.Ident $ clkName env)
      return $ Verilog.Instance mname parms name (clkPin : resetPin : portmap)
  | otherwise = return inst
fixTieResetClk x = return x

fixInstancesBitBlast :: Verilog.ModuleItem -> Verilog.ModuleItem
fixInstancesBitBlast a@(Verilog.Assign lhs expr) = Verilog.Instance "buff" [] ("buf_"++lhsname) [(Just "y", Just $ Verilog.Ident lhsname), (Just "a", Just expr')]
  where
    lhsname = case lhs of
      Verilog.LHS n -> n
      Verilog.LHSBit n (Verilog.Number idx) -> expandBusWireName n $ value idx
      _ -> error $ "unsupported verilog construct at lhs: " ++ show a
    expr' = case expr of
      Verilog.IdentBit n (Verilog.Number idx) -> Verilog.Ident . expandBusWireName n $ value idx
      x@(Verilog.Ident _) -> x
      _ -> error $ "unsupported verilog construct at rhs: " ++ show a
fixInstancesBitBlast (Verilog.Instance mname parms name portmap) =
  Verilog.Instance mname parms name $ map go portmap where
  go (x, Just (Verilog.IdentBit name (Verilog.Number idx))) = (x, Just . Verilog.Ident . expandBusWireName name $ value idx)
  go z = z
fixInstancesBitBlast x = x

vlogInputInstance :: [Verilog.Identifier] -> Verilog.ModuleItem
vlogInputInstance = Verilog.Input Nothing

wireName :: Wire -> String
wireName (Wire n)    = n
wireName (Bus _ _ n) = n

processModule :: (MonadReader PrgOptions m) => Verilog.Module -> m Verilog.Module
processModule m = do
  options <- ask
  let clkAndResetNames = [clkName options, resetName options]
  let clkAndReset = Set.fromList $ map Wire clkAndResetNames
  let inputs = vlogModuleInputs m Set.\\ clkAndReset
  let outputs = vlogModuleOutputs m
  let wires = vlogModuleAllWires m Set.\\ clkAndReset
  let (Verilog.Module mname _ mitems) = vlogModuleWithoutWires m
  let clkrstInst = vlogInputInstance clkAndResetNames
  let drWires = concatMap vlogDRWireInstance $ Set.elems wires
  let drInputs = concatMap vlogDRWireInputInst $ Set.elems inputs
  let drOutputs = concatMap vlogDRWireOutputInst $ Set.elems outputs
  let margs = concatMap (vlogDRWirePort . wireName) (Set.elems $ Set.union inputs outputs) ++ clkAndResetNames
  fixedDFF <- mapM (fixDffReset . fixInstancesBitBlast) mitems
  instances <- mapM fixTieResetClk fixedDFF
  let insts' = [clkrstInst] ++ drWires ++ drInputs ++ drOutputs ++ instances
  return $ Verilog.Module mname margs insts'

processVerilogFile :: (MonadReader PrgOptions m, MonadIO m) => FilePath -> m [Verilog.Module]
processVerilogFile path = readVerilogFile path >>= mapM processModule

processVerilogFiles :: (MonadReader PrgOptions m, MonadIO m) => [FilePath] -> m [Verilog.Module]
processVerilogFiles = fmap concat . mapM processVerilogFile
