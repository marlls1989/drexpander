{-# LANGUAGE FlexibleContexts #-}
import           Control.Monad.Reader
import           Data.LinearProgram.GLPK
import           Data.Map                (Map)
import qualified Data.Map                as Map
import           Data.Monoid
import           HBCN
import           Options.Applicative
import           Text.Printf
import           Text.Regex.TDFA

type LPRet = (ReturnCode, Maybe (Double, Map LPVar Double))

data PrgOptions = PrgOptions
  { inputFiles      :: [FilePath]
  , targetCycleTime :: Double
  , minimalDelay    :: Double
  , clockName       :: String
  , outputFile      :: FilePath
  , pathExceptions  :: Bool
  , debugSol        :: Bool
  } deriving (Show)

prgOptions :: Parser PrgOptions
prgOptions = PrgOptions
             <$> some (argument str
                        (metavar "FILES"
                         <> help "Input File Name"))
             <*> option auto (long "cycletime"
                              <> metavar "VALUE"
                              <> short 't'
                              <> help "Maximum Cycle Time Constraint")
             <*> option auto (long "mindelay"
                              <> short 'm'
                              <> metavar "VALUE"
                              <> value 0
                              <> help "Minimum Path Delay")
             <*> strOption (long "clock"
                            <> metavar "NAME"
                            <> short 'c'
                            <> value "clk"
                            <> help "Clock port name")
             <*> option str (long "output"
                             <> metavar "FILE"
                             <> short 'o'
                             <> value "ncl_constraints.sdc"
                             <> help "Output SDC File")
             <*> flag False True (long "path-exceptions"
                                  <> short 'x'
                                  <> help "Construct path exceptions to relax freeslack")
             <*> flag False True (long "debug"
                                  <> help "Print LP Variables Solution")



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
  let clkPeriod = vars Map.! PseudoClock
  let individual = pathExceptions opts
  return $ printf "create_clock -period %.3f [get_port {%s}]\n" clkPeriod (clockName opts) ++
    printf "set_input_delay -clock {%s} 0 [all_inputs]\n"   (clockName opts) ++
    printf "set_output_delay -clock {%s} 0 [all_outputs]\n" (clockName opts) ++
    if individual then
      concatMap maxDelay (filter (\(_, v) -> (v > clkPeriod + 0.001) || (v < clkPeriod - 0.001)) $ Map.toList vars)
    else []
  where
    maxDelay (FwDelay src dst, val)
      | (src =~ "port:") && (dst =~ "port:") =
        printf "set_max_delay -reset_path -from {%s} -to {%s} %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -reset_path -from {%s} -to {%s} %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from {%s} -to {%s} %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from {%s} -to {%s} %.3f\n" (falseRail src) (trueRail  dst) val
      | src =~ "port:" =
        printf "set_max_delay -reset_path -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -reset_path -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (trueRail  dst) val
      | otherwise =
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (trueRail  dst) val
    maxDelay (BwDelay src dst, val)
      | (src =~ "port:") && (dst =~ "port:") =
        printf "set_max_delay -reset_path -from {%s} -to {%s} %.3f\n" (ackRail src) (ackRail dst) val
      | src =~ "port:" =
        printf "set_max_delay -reset_path -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (ackRail src) (trueRail dst) val ++
        printf "set_max_delay -reset_path -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (ackRail src) (falseRail dst) val
      | dst =~ "port:" =
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to {%s} %.3f\n" (trueRail  src) (ackRail dst) val ++
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to {%s} %.3f\n" (falseRail src) (ackRail dst) val
      | otherwise =
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -reset_path -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (trueRail  dst) val
    maxDelay _ = []
    separateBus :: String -> (String, String, String)
    separateBus = (=~ "\\[[0-9]+\\]")
    ackRail s = let (n, b, _) = separateBus s in
          n ++ "_ack" ++ b
    trueRail s
      | s =~ "port:" = let (n, b, _) = separateBus s in
          n ++ "_t" ++ b
      | otherwise = s ++ "/t"
    falseRail s
      | s =~ "port:" = let (n, b, _) = separateBus s in
          n ++ "_f" ++ b
      | otherwise = s ++ "/f"

sdcContent err = errorWithoutStackTrace . printf "Could not solve LP: %s" $ show err

printSolution :: (MonadIO m) => LPRet -> m ()
printSolution (Data.LinearProgram.GLPK.Success, Just (_, vars)) = liftIO $
  mapM_  print $ Map.toList vars
printSlack err = errorWithoutStackTrace . printf "Could not solve LP: %s" $ show err

lpObjective :: LPRet -> Double
lpObjective (Data.LinearProgram.GLPK.Success, Just (o, _)) = o
lpObjective err = errorWithoutStackTrace . printf "Could not solve LP: %s" $ show err

prgMain :: ReaderT PrgOptions IO ()
prgMain = do
  opts <- ask
  hbcn <- hbcnFromFiles $ inputFiles opts
  let cycleTime = targetCycleTime opts
  let minDelay = minimalDelay opts
  let lp = constraintCycleTime hbcn cycleTime minDelay
  result <- liftIO $ glpSolveVars simplexDefaults lp
  sdc <- sdcContent result
  when (debugSol opts) $ do
    let lpfile = outputFile opts ++ ".lp"
    printSolution result
    liftIO $ writeLP lpfile lp
  liftIO $ if lpObjective result > 0.0005 then do
    printf "Writing constraints to %s\n" (outputFile opts)
    writeFile (outputFile opts) sdc
  else
    errorWithoutStackTrace "Deadlock Found in the Design, not writing constraints file"
