module Main where

import           Control.Monad.Reader
import           DRExpander
import           Options.Applicative

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


main :: IO ()
main = do
  let opts = info (prgOptions <**> helper)
             ( fullDesc
               <> progDesc "Prepares a netlist for dual-rail expansion"
               <> header "drexpand - Pulsar's dual-rail expansor")
  options <- execParser opts
  runReaderT prgMain options

prgMain :: ReaderT PrgOptions IO ()
prgMain = do
  env <- ask
  liftIO $ print env
