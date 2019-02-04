import           Control.Monad.Reader
import           Data.LinearProgram.GLPK
import           HBCN
import           Options.Applicative


data PrgOptions = PrgOptions
  { inputFiles      :: [FilePath]
  , targetCycleTime :: Double
  } deriving (Show)

prgOptions :: Parser PrgOptions
prgOptions = PrgOptions
             <$> some (argument str
                        (metavar "FILES"
                         <> help "Input File Name"))
             <*> option auto (long "cycletime"
                              <> short 'c'
                              <> help "Maximum Cycle Time Constraint")


main :: IO ()
main = do
  let opts = info (prgOptions <**> helper)
             ( fullDesc
               <> progDesc "Calculates the pseudo-clock period constraint for a given circuit"
               <> header "hbcnConstrainer - Pulsar Linear Programming HBCN constrainer")
  options <- execParser opts
  runReaderT prgMain options

hbcnFromFiles :: [FilePath] -> ReaderT PrgOptions IO HBCN
hbcnFromFiles files = do
  text <- mapM (liftIO . readFile) files
  let structure = map read . lines $ concat text
  return $ createHBCNFromStructure structure

prgMain :: ReaderT PrgOptions IO ()
prgMain = do
  opts <- ask
  hbcn <- hbcnFromFiles $ inputFiles opts
  let cycleTime = targetCycleTime opts
  let lp = constraintCycleTime hbcn cycleTime
  result <- liftIO $ glpSolveVars mipDefaults lp
  liftIO $ print result
