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
                        (metavar "input"
                         <> help "List of Structural Graph Files"))
             <*> option auto (long "cycletime"
                              <> metavar "time"
                              <> short 't'
                              <> help "Target Cycle Time Constraint")
             <*> option auto (long "mindelay"
                              <> short 'm'
                              <> metavar "time"
                              <> value (-1)
                              <> help "Minimum Path Delay, defaults to 10% of cycle time constraint")
             <*> strOption (long "clock"
                            <> metavar "portname"
                            <> short 'c'
                            <> value "clk"
                            <> help "Clock port name")
             <*> option str (long "output"
                             <> metavar "filename"
                             <> short 'o'
                             <> value "ncl_constraints.sdc"
                             <> help "Output SDC File, defaults to ncl_constraints.sdc")
             <*> flag True False (long "no-path-exceptions"
                                  <> help "Don't construct mindelay path exceptions")
             <*> flag False True (long "debug"
                                  <> help "Print LP Variables Solution and export lp problem")



main :: IO ()
main = do
  let opts = info (prgOptions <**> helper)
             ( fullDesc
               <> progDesc "Calculates the pseudo-clock constraints for a given circuit"
               <> header "hbcnConstrainer - Pulsar Linear Programming HBCN constrainer")
  options <- execParser opts
  runReaderT prgMain options

hbcnFromFiles :: Double -> [FilePath] -> ReaderT PrgOptions IO HBCN
hbcnFromFiles mindelay files = do
  text <- mapM (liftIO . readFile) files
  let structure = map read $ (lines . concat) text
  return $ createHBCNFromStructure (Just mindelay) structure

sdcContent :: (MonadReader PrgOptions m) => Map LPVar Double -> m String
sdcContent vars = do
  opts <- ask
  let clkPeriod = vars Map.! PseudoClock
  let individual = pathExceptions opts
  return $ printf "create_clock -period %.3f [get_port {%s}]\n" clkPeriod (clockName opts) ++
    (if individual then
      concatMap maxDelay (filter (\(_, v) -> (v > clkPeriod + 0.001) || (v < clkPeriod - 0.001)) $ Map.toList vars)
    else []) ++
    concatMap minDelay (Map.toList vars)
  where
    minDelay (MinDelay src dst, val)
      | (src =~ "port:") && (dst =~ "port:") =
        printf "# Reflexive Min Delay from %s to %s\n" src dst ++
        printf "set_min_delay -from {%s} -to {%s} %.3f\n" (trueRail  src) (ackRail dst) val ++
        printf "set_min_delay -from {%s} -to {%s} %.3f\n" (falseRail src) (ackRail dst) val ++
        if dst /= src then
          printf "set_min_delay -from {%s} -to {%s} %.3f\n" (ackRail src) (ackRail dst) val
        else []
      | src =~ "port:" =
        printf "# Reflexive Min Delay from %s to %s\n" src dst ++
        printf "set_min_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (ackRail src) (trueRail dst) val ++
        printf "set_min_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (ackRail src) (falseRail dst) val
      | dst =~ "port:" =
        printf "# Backward Delay from %s to %s\n" src dst ++
        printf "set_min_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to {%s} %.3f\n" (trueRail  src) (ackRail dst) val ++
        printf "set_min_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to {%s} %.3f\n" (falseRail src) (ackRail dst) val
      | otherwise =
        printf "# Reflexive Min Delay from %s to %s\n" src dst ++
        printf "set_min_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_min_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_min_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_min_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (trueRail  dst) val
    minDelay _ = []
    maxDelay (FwDelay src dst, val)
      | (src =~ "port:") && (dst =~ "port:") =
        printf "# Forward Delay from %s to %s\n" src dst ++
        printf "set_max_delay -from {%s} -to {%s} %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -from {%s} -to {%s} %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -from {%s} -to {%s} %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -from {%s} -to {%s} %.3f\n" (falseRail src) (trueRail  dst) val
      | src =~ "port:" =
        printf "# Forward Delay from %s to %s\n" src dst ++
        printf "set_max_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (trueRail  dst) val
      | otherwise =
        printf "# Forward Delay from %s to %s\n" src dst ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (trueRail  dst) val
    maxDelay (BwDelay src dst, val)
      | (src =~ "port:") && (dst =~ "port:") =
        printf "# Backward Delay from %s to %s\n" src dst ++
        printf "set_max_delay -from {%s} -to {%s} %.3f\n" (trueRail  src) (ackRail dst) val ++
        printf "set_max_delay -from {%s} -to {%s} %.3f\n" (falseRail src) (ackRail dst) val ++
        if dst /= src then
          printf "set_max_delay -from {%s} -to {%s} %.3f\n" (ackRail src) (ackRail dst) val
        else []
      | src =~ "port:" =
        printf "# Backward Delay from %s to %s\n" src dst ++
        printf "set_max_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (ackRail src) (trueRail dst) val ++
        printf "set_max_delay -from {%s} -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (ackRail src) (falseRail dst) val
      | dst =~ "port:" =
        printf "# Backward Delay from %s to %s\n" src dst ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to {%s} %.3f\n" (trueRail  src) (ackRail dst) val ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to {%s} %.3f\n" (falseRail src) (ackRail dst) val
      | otherwise =
        printf "# Backward Delay from %s to %s\n" src dst ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (trueRail  dst) val ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (falseRail dst) val ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (trueRail  src) (falseRail dst) val ++
        printf "set_max_delay -from [get_pin -of_objects {%s} -filter {is_clock_pin==true}] -to [get_pin -of_objects {%s} -filter {(is_clock_pin==false) && (direction==in)}] %.3f\n" (falseRail src) (trueRail  dst) val
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

printSolution :: (MonadIO m) => LPRet -> m ()
printSolution (Data.LinearProgram.GLPK.Success, Just (_, vars)) = liftIO $
  mapM_  print $ Map.toList vars
printSolution err = errorWithoutStackTrace . printf "Could not solve LP: %s" $ show err

lpObjective :: LPRet -> Double
lpObjective (Data.LinearProgram.GLPK.Success, Just (o, _)) = o
lpObjective err = errorWithoutStackTrace . printf "Could not solve LP: %s" $ show err

prgMain :: ReaderT PrgOptions IO ()
prgMain = do
  opts <- ask
  let cycleTime = targetCycleTime opts
  let minDelay = case minimalDelay opts of
        x | x < 0 -> cycleTime/10
          | otherwise -> x
  hbcn <- hbcnFromFiles minDelay $ inputFiles opts

  let ctLp = constraintCycleTime hbcn minDelay cycleTime
  when (debugSol opts) $ do
    let ctLpfile = outputFile opts ++ ".cycletime.lp"
    liftIO $ writeLP ctLpfile ctLp
  ctResult <- liftIO $ glpSolveVars simplexDefaults ctLp
  let ctVars = case ctResult of
        (Data.LinearProgram.GLPK.Success, Just (_, x)) -> x
        err -> errorWithoutStackTrace . printf "Could not solve Cycletime LP: %s" $ show err

  let minLp = constraintReflexivePaths hbcn ctVars
  when (debugSol opts) $ do
    let minLpfile = outputFile opts ++ ".mindelay.lp"
    liftIO $ writeLP minLpfile minLp
  minResult <- liftIO $ glpSolveVars simplexDefaults minLp
  let lpVars = case minResult of
        (Data.LinearProgram.GLPK.Success, Just (_, x)) -> x
        err -> errorWithoutStackTrace . printf "Could not solve Mindelay LP: %s" $ show err

  sdc <- sdcContent lpVars
  liftIO $ if lpObjective ctResult > 0.0005 then do
    printf "Writing constraints to %s\n" (outputFile opts)
    writeFile (outputFile opts) sdc
  else
    errorWithoutStackTrace "Deadlock Found in the Design, not writing constraints file"
