-- |
-- CLI entry point for @drexpander@ ("Pulsar's dual-rail expansor"). A thin
-- @optparse-applicative@ wrapper that parses 'PrgOptions' and runs the
-- 'DRExpander' pipeline, printing the transformed modules to stdout. The flows
-- invoke this through Genus's @shell@ command.
import           Control.Monad.Reader
import           DRExpander
import           Options.Applicative
import Data.Monoid

-- | Command-line parser: positional @FILES@ (one or more input netlists),
-- @--reset@\/@-r@ (default @"reset"@) and @--clock@\/@-c@ (default @"clk"@).
prgOptions :: Parser PrgOptions
prgOptions = PrgOptions
             <$> some (argument str (metavar "FILES"
                              <> help "Input File Name"))
             <*> strOption (long "reset"
                            <> short 'r'
                            <> value "reset"
                            <> help "Reset port name")
             <*> strOption (long "clock"
                           <> short 'c'
                           <> value "clk"
                           <> help "Clock port name")


-- | Parse the command line, then run the pipeline under the 'ReaderT'
-- environment.
main :: IO ()
main = do
  let opts = info (prgOptions <**> helper)
             ( fullDesc
               <> progDesc "Prepares a netlist for dual-rail expansion"
               <> header "drexpand - Pulsar's dual-rail expansor")
  options <- execParser opts
  runReaderT prgMain options

-- | The pipeline action: process every input file and print @show@ of each
-- resulting module, one per line, to stdout.
prgMain :: ReaderT PrgOptions IO ()
prgMain = do
  env <- ask
  modules <- processVerilogFiles $ verilogFiles env
  liftIO . putStr . unlines $ show <$> modules
