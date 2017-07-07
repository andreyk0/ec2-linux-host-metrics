{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Main where

import           App
import           Args
import           CPUInfo
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.AWS
import           Data.Either
import           Data.List.Split
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Df
import           Loadavg
import           Meminfo
import           Network.AWS.CloudWatch
import           Ntp
import           Stat
import           Types


main:: IO ()
main = runApp $ do
  args@Args{..} <- getArgs
  $(logDebugSH) args

  when (null argsDimensions) $ error "Please specify at least one dimension!"

  myInstanceID <- getInstanceID
  $(logDebugSH) myInstanceID

  cpuS <- cpuinfoSummary
  lAvg <- loadavg

  let lAvgMetrics = loadAvgMetricData <$> cpuS <*> lAvg

  dfMetrics <- diskFree <&> fmap dfMetricData

  memMetrics <- meminfo <&> fmap memMetricData

  ntpMetrics <- ntpQuery <&> fmap ntpMetricData

  cpuStats <- statsSinceLastRun <&> fmap cpuStatsMetricData

  allDimensionlessMetrics <- collateResults [ lAvgMetrics, dfMetrics, memMetrics, ntpMetrics, cpuStats ]

  let metricsWithDimensions dims = allDimensionlessMetrics <&> mdDimensions %~ (<> dims) -- adds a set of extra dimensions to all metrics

      fillInInstanceID dim = dim & dValue %~ \v -> case v of "INSTANCE_ID" -> myInstanceID -- replace magic INSTANCE_ID value everywhere
                                                             anyOther      -> anyOther

      allDimensions = argsDimensions <&> ( <&> fillInInstanceID )

      allMetrics = concat $ metricsWithDimensions <$> allDimensions

  if argsPublishMetrics
    then do $(logDebugSH) allMetrics
            forM_ (chunksOf 20 allMetrics) $ \ms -> -- split into chunks to avoid hitting the call size limit
                void . send $ putMetricData "System/Linux" & pmdMetricData .~ ms

    else forM_ allMetrics (liftIO . print)


-- Handle results of metric collection actions that may have failed.
-- Log and count errors, convert them to an error count metric.
-- Pass successfully collected metrics through.
collateResults :: [Either String [MetricDatum]]
               -> App [MetricDatum]
collateResults results = do
  let (errs, metricResults) = partitionEithers results
      allMetricResults = concat metricResults

      agentMetricName = "Ec2LinuxHostMetricsAgent"

      agentMetrics = [ metricDatum agentMetricName & mdValue .~ (Just . fromIntegral) (length allMetricResults)
                                                   & mdUnit .~ Just Count
                                                   & mdDimensions .~ [ dimension "result" "success" ]

                     , metricDatum agentMetricName & mdValue .~ (Just . fromIntegral) (length errs)
                                                   & mdUnit .~ Just Count
                                                   & mdDimensions .~ [ dimension "result" "error" ]
                     ]

  forM_ errs $ \e -> $(logError) (T.pack e)

  return $ agentMetrics <> allMetricResults


dfMetricData :: Df
             -> [MetricDatum]
dfMetricData dfs = concat $ dfs <&> dfFsMetricData

dfFsMetricData :: DfFs
               -> [MetricDatum]
dfFsMetricData DfFs{..} = [
    metricDatum "DiskSpaceUsed"        & mdValue .~ (Just . fromIntegral) dffsUsed
                                       & mdUnit .~ Just Kilobytes

  , metricDatum "DiskSpaceAvailable"   & mdValue .~ (Just . fromIntegral) dffsAvailable
                                       & mdUnit .~ Just Kilobytes

  , metricDatum "DiskSpaceUtilization" & mdValue .~ Just dsUtil
                                       & mdUnit .~ Just Percent

  ] <&> mdDimensions %~ (dimension "Mountpoint" dffsMountpoint :)

  where dsUtil = let cap = if dffsCapacity == 0
                           then 1 -- to avoid /0
                           else fromIntegral dffsCapacity
                  in 100 * fromIntegral dffsUsed / cap


memMetricData :: Meminfo
              -> [MetricDatum]
memMetricData mis =
  let misMap = Map.fromList $ (\ mie@MeminfoEntry{..} -> (mieKey, mie)) <$> mis
      meminfoEntry k = Map.lookup k misMap

      memTotal = meminfoEntry "MemTotal"
      memFree  = meminfoEntry "MemFree"
      buffers  = meminfoEntry "Buffers"
      cached   = meminfoEntry "Cached"

      memAvailable = meminfoEntry "MemAvailable"

      -- total memory, guarded against 0 just in case
      memTotalValue = do mt <- mieVal <$> memTotal
                         return $ fromIntegral $ if mt == 0 then 1 else mt

      percentOfTotalMemory x = do x' <- mieVal <$> x
                                  t  <- memTotalValue
                                  return $ 100 * fromIntegral x' / t

      memUsed = do t <- mieVal <$> memTotal
                   f <- mieVal <$> memFree
                   b <- mieVal <$> buffers
                   c <- mieVal <$> cached

                   return $ MeminfoEntry "MemUsed" ( t - f - b - c ) -- MemTotal - (MemFree + Buffers + Cached)


   in catMaybes [ -- report whatever entries we can find
        memEntryBytesDatum <$> memTotal
      , memEntryBytesDatum <$> memFree
      , memEntryBytesDatum <$> memAvailable
      , memPercentDatum "MemAvailablePercent" <$> percentOfTotalMemory memAvailable
      , memEntryBytesDatum <$> memUsed
      , memPercentDatum "MemUsedPercent" <$> percentOfTotalMemory memUsed
      ]

  where memEntryBytesDatum MeminfoEntry{..} =
          metricDatum mieKey & mdValue .~ (Just . fromIntegral) mieVal
                             & mdUnit .~ Just Bytes

        memPercentDatum k v =
          metricDatum k & mdValue .~ Just v
                        & mdUnit .~ Just Percent


-- Reporting absolute values of offset/jitter,
-- to make it easier to set up alerts around this.
ntpMetricData :: Ntp
              -> [MetricDatum]
ntpMetricData Ntp{..} = [
    metricDatum "NtpOffsetAbs" & mdValue .~ Just (abs ntpOffset)
                               & mdUnit .~ Just Milliseconds

  , metricDatum "NtpJitterAbs" & mdValue .~ Just (abs ntpJitter)
                               & mdUnit .~ Just Milliseconds
  ]


-- Loadavg adjusted by the number of CPU cores,
-- to normalize output across nodes of different type
loadAvgMetricData :: CPUInfoSummary
                  -> Loadavg
                  -> [MetricDatum]
loadAvgMetricData CPUInfoSummary{..} Loadavg{..} = [
    lavgCPUMetric lavgCPU1 "1"

  , lavgCPUMetric lavgCPU5 "5"

  , lavgCPUMetric lavgCPU10 "10"

  , metricDatum "ProcessesRunning" & mdValue .~ Just (fromIntegral lavgProcRunning)
                                   & mdUnit .~ Just Count

  , metricDatum "ProcessesTotal"   & mdValue .~ Just (fromIntegral lavgProcTotal)
                                   & mdUnit .~ Just Count
  ]

  where perCorePercent x = 100 * x / fromIntegral (if cpuisNumPhysCores == 0 then 1 else cpuisNumPhysCores)

        lavgCPUMetric x n = metricDatum ("CPULoadAvgPerCore" <> n) & mdValue .~ Just (perCorePercent x)
                                                                   & mdUnit .~ Just Percent



-- CPU stats from /proc/stat since last run.
-- No output on the first run, only diffs from last (saved) state are useful.
-- https://stackoverflow.com/questions/23367857/accurate-calculation-of-cpu-usage-given-in-percentage-in-linux
cpuStatsMetricData :: (Maybe Stat, Stat)
                   -> [MetricDatum]
cpuStatsMetricData (Nothing, _) = []
cpuStatsMetricData (Just sPrev, sCurr) = [
    metricDatum "CPUUtilizationPercent" & mdValue .~ Just cpuUtilizationPercent
                                        & mdUnit .~ Just Percent
  ]

  where idleTime Stat{..} = statCPUIdle + statCPUIOWait
        nonIdleTime Stat{..} = statCPUUser + statCPUNice + statCPUSystem + statCPUIRQ + statCPUSoftIRQ + statCPUSteal
        totalTime s = idleTime s + nonIdleTime s

        totalTimeDiff = totalTime sCurr - totalTime sPrev
        idleTimeDiff = idleTime sCurr - idleTime sPrev

        cpuUtilizationPercent = 100 * fromIntegral (totalTimeDiff - idleTimeDiff) /  fromIntegral totalTimeDiff
