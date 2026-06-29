{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists  #-}

{-|
Module      : DRExpander
Description : Pulsar's dual-rail expansor — the whole library.
Copyright   : (c) Marcos Luiggi Lemos Sartori, 2018-2025
License     : MIT

The complete library behind the @drexpander@ executable ("Pulsar's dual-rail
expansor"), a tool source in the Pulsar ecosystem for QDI / SDDS-NCL (NULL
Convention Logic) asynchronous circuits.

= Role in the flow

@drexpander@ reads a single-rail (one wire per logical net) Verilog netlist
and rewrites it into a form the Pulsar synthesis flows can dual-rail expand into
NCL cells. The flows invoke the prebuilt binary through Genus:

@
shell drexpander \${OUTDIR}\/\${DESIGN}.v > \${OUTDIR}\/ncl_\${DESIGN}.v
@

= What it does (pipeline)

Per module, 'processModule' performs:

  1. /Introspect/ the parsed module — collect input\/output\/wire declarations
     as a 'Set' of 'Wire's (see 'vlogModuleInputs' & friends).
  2. /Separate clk\/reset/ — the clock and reset nets are removed from the
     dual-rail sets so they stay ordinary single-rail control wires.
  3. /Strip declarations/ ('vlogModuleWithoutWires') and re-synthesise them as
     dual-rail artifacts: an internal 'drwire' per net, plus a 'drinput' \/
     'droutput' IO adaptor per port.
  4. /Fix up instances/ — bit-blast indexed bus references into flat per-bit
     names, rewrite continuous @assign@s into explicit @buff@ cells, and inject
     @.clk@\/@.reset@ pins into @dff@\/@tielo@\/@tiehi@ instances.
  5. /Emit/ — the caller @show@s the rebuilt 'Verilog.Module' to stdout.

= The dual-rail contract (@_t@ \/ @_f@ \/ @_ack@)

Each single-rail net @n@ becomes a dual-rail triple under the SDDS-NCL encoding:
a "true" rail @n_t@, a "false" rail @n_f@, and a return-to-zero acknowledge
@n_ack@. The IO adaptor instance names and their @.t@\/@.f@\/@.ack@\/@.drw@
ports must stay in sync with the flow's SystemVerilog adaptor modules
(@tech\/*alho.sv@) — this module is the generator side of that interface.
-}
module DRExpander where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.BitVec
import           Data.Set               (Set)
import qualified Data.Set               as Set
import qualified Language.Verilog       as Verilog
import Data.List

-- | A logical net in the source netlist: either a scalar @Wire name@, or a
-- @Bus hi lo name@ with inclusive index bounds (the bounds may be given in
-- either order — @[hi:lo]@ or @[lo:hi]@; consumers normalise with 'min'\/'max').
data Wire = Wire String                  -- ^ A single-bit net @name@.
          | Bus Integer Integer String   -- ^ A multi-bit bus @name[hi:lo]@.
          deriving (Eq, Ord)

-- | The 'ReaderT' environment threaded through the pipeline: the input files
-- and the user-overridable names of the clock and reset ports.
data PrgOptions = PrgOptions
  { verilogFiles :: [FilePath]  -- ^ Input Verilog netlists to process.
  , resetName    :: String      -- ^ Reset port name (CLI @--reset@, default @reset@).
  , clkName      :: String      -- ^ Clock port name (CLI @--clock@, default @clk@).
  } deriving (Show)

-- | Bit-blast a bus into its per-bit scalar 'Wire's, one per index in the
-- inclusive range (orientation-independent). A scalar 'Wire' is returned
-- unchanged as a singleton.
bitBlastWire :: Wire -> [Wire]
bitBlastWire (Bus x y name) = map (Wire . expandBusWireName name) [x'..y'] where
  x' = min x y
  y' = max x y
bitBlastWire x              = [x]

-- | Flat name for one bit of a bus: @name ++ "_" ++ idx@ (e.g. @"a"@, @3@ → @"a_3"@).
expandBusWireName :: String -> Integer -> String
expandBusWireName name idx = name ++ "_" ++ show idx

-- | Read and parse a single Verilog file into its list of modules (no
-- preprocessor defines are supplied).
readVerilogFile :: (MonadIO m) => FilePath -> m [Verilog.Module]
readVerilogFile path = do
  s <- liftIO $ readFile path
  return $ Verilog.parseFile [] path s

-- | Read and parse several files, concatenating all their modules.
readVerilogFiles :: (MonadIO m) => [FilePath] -> m [Verilog.Module]
readVerilogFiles = fmap concat . mapM readVerilogFile

-- $introspection
-- The @vlogModule*@ helpers walk a parsed 'Verilog.Module' and collect its
-- declarations as a 'Set' of 'Wire's. They all match the same @verilog@-package
-- AST shapes:
--
--   * @Verilog.Input@ \/ @Verilog.Output@ \/ @Verilog.Wire@ module items;
--   * a scalar declaration carries @Nothing@ for its range and becomes a 'Wire';
--   * a ranged declaration carries @Just (Number hi, Number lo)@ and becomes a
--     'Bus', with each bound decoded from its @BitVec@ via 'value'.

-- | All declared module inputs, as scalar 'Wire's or ranged 'Bus'es.
vlogModuleInputs :: Verilog.Module -> Set Wire
vlogModuleInputs (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Input Nothing xs)       = map Wire xs
    go (Verilog.Input (Just (Verilog.Number x, Verilog.Number y)) xs) = map (Bus x' y') xs where
      y' = value y
      x' = value x
    go _                               = []

-- | All declared module outputs, as scalar 'Wire's or ranged 'Bus'es.
vlogModuleOutputs :: Verilog.Module -> Set Wire
vlogModuleOutputs (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Output Nothing xs)       = map Wire xs
    go (Verilog.Output (Just (Verilog.Number x, Verilog.Number y)) xs) = map (Bus x' y') xs where
      y' = value y
      x' = value x
    go _                               = []

-- | All declared internal @wire@s. (The @verilog@ AST pairs each name with an
-- optional initialiser; only the name, @fst@, is kept.)
vlogModuleWires :: Verilog.Module -> Set Wire
vlogModuleWires (Verilog.Module _ _ items) = Set.fromList $ concatMap go items
  where
    go (Verilog.Wire Nothing xs)       = map (Wire . fst) xs
    go (Verilog.Wire (Just (Verilog.Number x, Verilog.Number y)) xs) = map (Bus x' y') xs' where
      y' = value y
      x' = value x
      xs' = map fst xs
    go _                               = []

-- | Strictly-internal wires: declared @wire@s that are not also ports.
vlogModuleIntWires :: Verilog.Module -> Set Wire
vlogModuleIntWires mod = wires Set.\\ Set.union inputs outputs
  where
    wires = vlogModuleWires mod
    inputs = vlogModuleInputs mod
    outputs = vlogModuleOutputs mod

-- | Every net in the module: declared wires together with all ports. This is
-- the set that each gets an internal 'drwire' instance in 'processModule'.
vlogModuleAllWires :: Verilog.Module -> Set Wire
vlogModuleAllWires mod = Set.unions (wires : inputs : outputs : [])
  where
    wires :: Set Wire
    wires = vlogModuleWires mod
    inputs :: Set Wire
    inputs = vlogModuleInputs mod
    outputs :: Set Wire
    outputs = vlogModuleOutputs mod

-- | Strip all @input@\/@output@\/@wire@ declarations (and clear the port list),
-- leaving only the body items (instances, assigns, …). 'processModule' uses this
-- to clear the original interface before re-synthesising the dual-rail one.
vlogModuleWithoutWires :: Verilog.Module -> Verilog.Module
vlogModuleWithoutWires (Verilog.Module name _ items) =
  Verilog.Module name [] $ filter p items where
  p (Verilog.Input _ _)  = False
  p (Verilog.Output _ _) = False
  p (Verilog.Wire _ _)   = False
  p _                    = True

-- | Emit a single dual-rail IO adaptor instance binding the four interface
-- ports — @.t(name_t)@, @.f(name_f)@, @.ack(name_ack)@ and @.drw(name)@ — for
-- the adaptor module @mname@ (@"drinput"@ or @"droutput"@). The instance is
-- named @"i" ++ name@. With @Nothing@ it wires whole scalar nets; with
-- @Just idx@ it wires the single bus bit @[idx]@ of the @_t@\/@_f@\/@_ack@
-- rails to the flat per-bit @drwire@ (named via 'expandBusWireName').
--
-- The port names here are the contract with the flow's @tech\/*alho.sv@ adaptor
-- modules and must not drift from them.
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

-- | Emit the internal @drwire@ instance(s) representing a net in dual-rail
-- form: one @drwire <name> ()@ per scalar, and one per bit (using flat per-bit
-- names) for a bus.
vlogDRWireInstance :: Wire -> [Verilog.ModuleItem]
vlogDRWireInstance (Wire name) = [Verilog.Instance "drwire" [] name []]
vlogDRWireInstance bus = concatMap vlogDRWireInstance $ bitBlastWire bus

-- | The three dual-rail rail names for a net: @[name_t, name_f, name_ack]@.
vlogDRWirePort :: Verilog.Identifier -> [Verilog.Identifier]
vlogDRWirePort name = map (name ++) ["_t", "_f", "_ack"]

-- | Build the dual-rail interface for a module __input__ net: declare the true
-- and false rails as module inputs, the acknowledge as a module output, and
-- attach a @drinput@ adaptor.
--
-- Note the @_ack@ direction __asymmetry__ versus 'vlogDRWireOutputInst': for an
-- input the data rails flow in (@input  a_t, a_f;@) while the ack flows back out
-- (@output a_ack;@). An output mirrors this — its data rails are outputs and its
-- ack is an input — so the two functions are deliberate duals.
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


-- | Build the dual-rail interface for a module __output__ net: the dual of
-- 'vlogDRWireInputInst'. The true\/false rails become module outputs, the
-- acknowledge becomes a module input, and a @droutput@ adaptor is attached.
-- (See the @_ack@ direction asymmetry note on 'vlogDRWireInputInst'.)
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

-- | Remove duplicates from a list (order is not preserved; result is sorted
-- via 'Set').
dedupList :: (Ord a) => [a] -> [a]
dedupList = Set.toList . Set.fromList

-- | Inject the reset connection into @dff@ instances: prepend
-- @.reset(<resetName>)@ to the port map of any @dff@ instance, using the reset
-- name from the 'PrgOptions' environment. Non-@dff@ items pass through.
fixDffReset :: (MonadReader PrgOptions m) => Verilog.ModuleItem -> m Verilog.ModuleItem
fixDffReset inst@(Verilog.Instance mname parms name portmap)
  | mname == "dff" = do
      env <- ask
      let resetPin = (Just "reset", Just . Verilog.Ident $ resetName env)
      return $ Verilog.Instance mname parms name (resetPin : portmap)
  | otherwise = return inst
fixDffReset x = return x

-- | Inject clock and reset connections into constant-driver instances: prepend
-- @.clk(<clkName>)@ and @.reset(<resetName>)@ to the port map of any @tielo@ or
-- @tiehi@ instance. Other items pass through.
fixTieResetClk :: (MonadReader PrgOptions m) => Verilog.ModuleItem -> m Verilog.ModuleItem
fixTieResetClk inst@(Verilog.Instance mname parms name portmap)
  | mname == "tielo" || mname == "tiehi" = do
      env <- ask
      let resetPin = (Just "reset", Just . Verilog.Ident $ resetName env)
      let clkPin = (Just "clk", Just . Verilog.Ident $ clkName env)
      return $ Verilog.Instance mname parms name (clkPin : resetPin : portmap)
  | otherwise = return inst
fixTieResetClk x = return x

-- | Rewrite module items so that bus references become flat per-bit names and
-- continuous assignments become explicit buffer cells. There are three cases,
-- dispatched by AST shape:
--
--   1. /Continuous assign/ @assign lhs = expr@ is replaced by an explicit
--      @buff@ cell instance named @"buf_" ++ lhs@, wiring @.y(lhs)@ and
--      @.a(expr)@. (The flow needs a real cell, not a netlist assign.)
--   2. /Indexed bus references/ — an @IdentBit n idx@ on either side is resolved
--      to the flat per-bit name @n_idx@ (via 'expandBusWireName'), matching the
--      bits emitted by 'bitBlastWire'.
--   3. /Instance port maps/ — any @IdentBit@ connection in an instance's port
--      map is likewise flattened to its per-bit name.
--
-- All other items pass through unchanged.
--
-- __Limitation:__ the @assign@ case only understands a plain identifier or a
-- single indexed bit on each side; any other LHS\/RHS construct (slices,
-- expressions, concatenations, …) triggers a partial 'error'. This is a known
-- restriction on the netlists this tool accepts, not a general Verilog rewriter.
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

-- | Declare a list of identifiers as plain scalar module inputs (no range).
-- Used to re-declare the clock and reset ports, which stay single-rail.
vlogInputInstance :: [Verilog.Identifier] -> Verilog.ModuleItem
vlogInputInstance = Verilog.Input Nothing

-- | Project the base name out of a 'Wire' or 'Bus'.
wireName :: Wire -> String
wireName (Wire n)    = n
wireName (Bus _ _ n) = n

-- | The heart of the tool: rewrite one parsed module into its dual-rail-ready
-- form.
--
-- Steps:
--
--   * Build the clk\/reset set and __subtract__ it from the inputs and from the
--     all-wires set so the clock and reset are never dual-rail expanded — they
--     stay ordinary single-rail control nets.
--   * Strip the original declarations with 'vlogModuleWithoutWires', then
--     re-synthesise: a 'drwire' per net, a 'drinput' per input, a 'droutput'
--     per output, and a new port list of @_t@\/@_f@\/@_ack@ triples (plus the
--     bare clk\/reset names).
--   * Run the instance fix-ups as composed passes:
--     @mapM (fixDffReset . fixInstancesBitBlast)@ then @mapM fixTieResetClk@.
--     __Ordering matters__: bit-blasting runs __before__ reset injection so the
--     injected @.reset@\/@.clk@ pins are not themselves bit-blasted.
--
-- The exact ordering of the assembled item list
-- (@clkrst : drwires : drinputs : droutputs : instances@) is pinned by the
-- golden test (@test\/golden\/design.processed.v@); do not reorder it.
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

-- | Read one file and run 'processModule' over each of its modules.
processVerilogFile :: (MonadReader PrgOptions m, MonadIO m) => FilePath -> m [Verilog.Module]
processVerilogFile path = readVerilogFile path >>= mapM processModule

-- | Read and process several files, concatenating all resulting modules.
processVerilogFiles :: (MonadReader PrgOptions m, MonadIO m) => [FilePath] -> m [Verilog.Module]
processVerilogFiles = fmap concat . mapM processVerilogFile
