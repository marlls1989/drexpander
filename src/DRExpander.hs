module DRExpander where

import           Control.Monad.IO.Class
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

