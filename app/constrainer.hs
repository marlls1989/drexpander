{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.Reader
import           Data.LinearProgram.GLPK
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           HBCN
import           Options.Applicative
import           Text.Printf

type LPRet = (ReturnCode, Maybe (Double, Map LPVar Double))

data PrgOptions = PrgOptions
  { inputFiles      :: [FilePath]
  , targetCycleTime :: Double
  , clockName       :: String
  , outputFile      :: FilePath
  , debugSlacks     :: Bool
  } deriving (Show)

prgOptions :: Parser PrgOptions
prgOptions = PrgOptions
             <$> some (argument str
                        (metavar "FILES"
                         <> help "Input File Name"))
             <*> option auto (long "cycletime"
                              <> metavar "VALUE"
                              <> short 't'
                              <> value 1
                              <> help "Maximum Cycle Time Constraint")
             <*> strOption (long "clock"
                            <> metavar "NAME"
                            <> short 'c'
                            <> value "clk"
                            <> help "Clock port name")
             <*> option str (long "output"
                             <> metavar "FILE"
                             <> short 'o'
                             <> value "constraints.sdc"
                             <> help "Output SDC File")
             <*> flag False True (long "slacks"
                                  <> help "Print Free Slack")



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
  let structure = map read $ (lines . concat) text
  return $ createHBCNFromStructure structure

sdcContent :: (MonadReader PrgOptions m) => LPRet -> m String
sdcContent (Data.LinearProgram.GLPK.Success, Just (_, vars)) = do
  opts <- ask
  let clkPeriod = 100 * (vars Map.! DelayFactor)
  return $ printf "create_clock -period %.3f [get_port {%s}]\n" clkPeriod (clockName opts) ++
    printf "set_input_transition -clock {%s} 0 [all_inputs]\n"   (clockName opts) ++
    printf "set_output_transition -clock {%s} 0 [all_outputs]\n" (clockName opts) ++
    (concatMap maxDelay . filter
      (\(x, val) -> case x of
          (Delay _ _) -> True
          _           -> False
      ) $ Map.toList vars)
  where
    maxDelay (Delay src dst, val) =
      printf "set_max_delay -reset_path -from {%s_t} -to {%s_t} %.3f\n" src dst val ++
      printf "set_max_delay -reset_path -from {%s_f} -to {%s_f} %.3f\n" src dst val ++
      printf "set_max_delay -reset_path -from {%s_t} -to {%s_f} %.3f\n" src dst val ++
      printf "set_max_delay -reset_path -from {%s_f} -to {%s_t} %.3f\n" src dst val ++
      printf "set_max_delay -reset_path -from {%s_t} -to {%s_ack} %.3f\n" src dst val ++
      printf "set_max_delay -reset_path -from {%s_f} -to {%s_ack} %.3f\n" src dst val

sdcContent err = errorWithoutStackTrace . printf "Could not solve LP: %s" $ show err

printSlack :: (MonadIO m) => LPRet -> m ()
printSlack (Data.LinearProgram.GLPK.Success, Just (_, vars)) = liftIO $
  mapM_  (\(x, v) ->
            if v < 0.0005 then return ()
            else case x of
              BwSlack s d -> printf "Backward propagation slack from %s to %s: %.3f\n" s d v
              FwSlack s d -> printf "Forward propagation slack from %s to %s: %.3f\n" s d v
              _           -> return ()) $ Map.toList vars
printSlack err = errorWithoutStackTrace . printf "Could not solve LP: %s" $ show err

prgMain :: ReaderT PrgOptions IO ()
prgMain = do
  opts <- ask
  hbcn <- hbcnFromFiles $ inputFiles opts
  let cycleTime = targetCycleTime opts
  let lp = constraintCycleTime hbcn cycleTime
  result <- liftIO $ glpSolveVars simplexDefaults lp
  sdc <- sdcContent result
  liftIO $ writeFile (outputFile opts) sdc
  when (debugSlacks opts) $
    printSlack result
