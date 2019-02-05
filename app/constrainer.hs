import           Control.Monad.Reader
import           Data.LinearProgram.GLPK
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           HBCN
import           Options.Applicative
import           Text.Printf

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
                              <> value 1
                              <> help "Maximum Cycle Time Constraint")


main :: IO ()
main = do
  let opts = info (prgOptions <**> helper)
             ( fullDesc
               <> progDesc "Calculates the pseudo-clock period constraint for a given circuit"
               <> header "hbcnConstrainer - Pulsar Linear Programming HBCN constrainer")
  options <- execParser opts
  runReaderT prgMain options

hbcnFromFiles :: (MonadIO m) => [FilePath] -> m HBCN
hbcnFromFiles files = do
  text <- mapM (liftIO . readFile) files
  let structure = map read $ (lines . concat) text
  return $ createHBCNFromStructure structure

prettyPrint :: (MonadIO m) => (ReturnCode, Maybe (Double, Map LPVar Double)) -> m ()
prettyPrint (Data.LinearProgram.GLPK.Success, Just (_, vars)) = liftIO $ do
  let slacks = filter (\(x, val) -> case x of
                          (FreeSlack _ _) -> val >= 0.0005
                          _               -> False
                      ) $ Map.toList vars
  printf "Found pseudo clock constraint: %.3g\n" $ vars Map.! ClkPeriod
  mapM_  slackString slacks
  where
    slackString ((FreeSlack src dst), val) =
      printf "Free Slack from %s to %s: %.3g\n"
      (transitionString src) (transitionString dst) val
    transitionString :: Transition -> String
    transitionString (DataTrans n) = printf "Data Transition in \"%s\"" n
    transitionString (NullTrans n) = printf "Null Transition in \"%s\"" n

prettyPrint err = errorWithoutStackTrace $ show err

prgMain :: ReaderT PrgOptions IO ()
prgMain = do
  opts <- ask
  hbcn <- hbcnFromFiles $ inputFiles opts
  let cycleTime = targetCycleTime opts
  let lp = constraintCycleTime hbcn cycleTime
  result <- liftIO $ glpSolveVars simplexDefaults lp
  prettyPrint result
