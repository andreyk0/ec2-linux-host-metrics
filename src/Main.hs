{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE ViewPatterns       #-}


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
import           Data.Text (Text)
import qualified Data.Text as T
import           Df
import           Loadavg
import           Meminfo
import           NetStat
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

  let gatherData cliFlag action = if cliFlag
                                  then action
                                  else pure $ Right []

  lAvgMetrics <- gatherData argsCPU $ do
    cpuS <- cpuinfoSummary
    lAvg <- loadavg
    pure $ loadAvgMetricData <$> cpuS <*> lAvg

  cpuStats <- gatherData argsCPU $ statsSinceLastRun <&> fmap cpuStatsMetricData

  memMetrics <- gatherData argsMemory $ meminfo <&> fmap memMetricData

  let filterMountPoints dfs = let mps = fromMaybe [] argsDisk
                               in if null mps
                                  then dfs
                                  else filter (\ DfFs{..} -> elem dffsMountpoint mps ) dfs

  dfMetrics <- gatherData (isJust argsDisk) $ diskFree <&> fmap (dfMetricData . filterMountPoints)

  ntpMetrics <- gatherData argsNTP $ ntpQuery <&> fmap ntpMetricData

  parsedNetStats <- netStatsSinceLastRun

  let nsGauges = fmap ((netStatGauges args) . netStatLookup . snd) parsedNetStats
      nsCounterDiffs = fmap ((netStatCounterDiffs args) . netStatDiffs) parsedNetStats

  allDimensionlessMetrics <- collateResults [ lAvgMetrics, dfMetrics, memMetrics, ntpMetrics, cpuStats, nsGauges, nsCounterDiffs ]

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
    metricDatum "CPUUtilizationPercent"       & mdValue .~ Just cpuUtilizationPercent
                                              & mdUnit .~ Just Percent

  , metricDatum "CPUUtilizationUserPercent"   & mdValue .~ Just (cpuFieldPercentTotal statCPUUser)
                                              & mdUnit .~ Just Percent

  , metricDatum "CPUUtilizationNicePercent"   & mdValue .~ Just (cpuFieldPercentTotal statCPUNice)
                                              & mdUnit .~ Just Percent

  , metricDatum "CPUUtilizationSystemPercent" & mdValue .~ Just (cpuFieldPercentTotal statCPUSystem)
                                              & mdUnit .~ Just Percent

  , metricDatum "CPUInterrupts"               & mdValue .~ Just (fieldDiff statCPUIntr)
                                              & mdUnit .~ Just Count

  , metricDatum "CPUContextSwitches"          & mdValue .~ Just (fieldDiff statCPUCtxt)
                                              & mdUnit .~ Just Count

  , metricDatum "ProcessesCreated"            & mdValue .~ Just (fieldDiff statProcsCreated)
                                              & mdUnit .~ Just Count
  ]

  where idleTime Stat{..} = statCPUIdle + statCPUIOWait
        nonIdleTime Stat{..} = statCPUUser + statCPUNice + statCPUSystem + statCPUIRQ + statCPUSoftIRQ + statCPUSteal
        totalTime s = idleTime s + nonIdleTime s

        totalTimeDiff = totalTime sCurr - totalTime sPrev
        idleTimeDiff = idleTime sCurr - idleTime sPrev

        cpuUtilizationPercent = 100 * fromIntegral (totalTimeDiff - idleTimeDiff) /  fromIntegral totalTimeDiff

        fieldDiff f = fromIntegral $ f sCurr - f sPrev
        cpuFieldPercentTotal f = 100 * fieldDiff f / fromIntegral totalTimeDiff



-- Generates metrics from the current values of gauge-like netstat fields.
--  https://www.ietf.org/rfc/rfc1213.txt
netStatGauges :: Args
              -> NetStatLookup
              -> [MetricDatum]
netStatGauges args (netStatMetricsBuilders args -> (nsIpCounterMetric, nsTcpCounterMetric, _)) = catMaybes $ -- report whatever entries we can find
  (nsIpCounterMetric <$> [
    "ReasmTimeout"
  ]) ++ (nsTcpCounterMetric <$> [
    "CurrEstab"
  ])


-- Generates metrics from the previous/current state diffs of the counter-like netstat fields.
--  https://www.ietf.org/rfc/rfc1213.txt
-- Many fields will typically stay at 0, we only send metrics when they are non-0.
netStatCounterDiffs :: Args
                    -> NetStatLookup
                    -> [MetricDatum]
netStatCounterDiffs args (netStatMetricsBuilders args -> (nsIpCounterMetric, nsTcpCounterMetric, nsUdpCounterMetric)) = catMaybes $ -- report whatever entries we can find
  (nsIpCounterMetric <$> [
    "ForwDatagrams"
  , "FragCreates"
  , "FragFails"
  , "FragOKs"
  , "InAddrErrors"
  , "InDelivers"
  , "InDiscards"
  , "InHdrErrors"
  , "InReceives"
  , "InUnknownProtos"
  , "OutDiscards"
  , "OutNoRoutes"
  , "OutRequests"
  , "ReasmFails"
  , "ReasmOKs"
  , "ReasmReqds"
  ]) ++ (nsTcpCounterMetric <$> [
    -- from /proc/net/snmp
    "ActiveOpens"
  , "AttemptFails"
  , "EstabResets"
  , "InCsumErrors"
  , "InErrs"
  , "InSegs"
  , "OutRsts"
  , "OutSegs"
  , "PassiveOpens"
  , "RetransSegs"

    -- from /proc/net/netstat
    -- https://github.com/ecki/net-tools/blob/master/statistics.c  -- field descriptions

    -- , "BusyPollRxPackets"
    -- , "DelayedACKLocked"
    -- , "DelayedACKLost"
    -- , "DelayedACKs"
    -- , "EmbryonicRsts"
    -- , "IPReversePathFilter"
  , "ListenDrops"
  , "ListenOverflows"
  , "LockDroppedIcmps"
  , "OfoPruned"
  , "OutOfWindowIcmps"
  , "PAWSActive"
  , "PAWSEstab"
  , "PAWSPassive"
  , "PruneCalled"
  , "RcvPruned"
  , "SyncookiesFailed"
  , "SyncookiesRecv"
  , "SyncookiesSent"
  , "TCPACKSkippedChallenge"
  , "TCPACKSkippedFinWait2"
  , "TCPACKSkippedPAWS"
  , "TCPACKSkippedSeq"
  , "TCPACKSkippedSynRecv"
  , "TCPACKSkippedTimeWait"
  , "TCPAbortFailed"
  , "TCPAbortOnClose"
  , "TCPAbortOnData"
  , "TCPAbortOnLinger"
  , "TCPAbortOnMemory"
  , "TCPAbortOnTimeout"
    -- , "TCPAutoCorking"
    -- , "TCPBacklogDrop"
    -- , "TCPChallengeACK"
    -- , "TCPDSACKIgnoredNoUndo"
    -- , "TCPDSACKIgnoredOld"
    -- , "TCPDSACKOfoRecv"
    -- , "TCPDSACKOfoSent"
    -- , "TCPDSACKOldSent"
    -- , "TCPDSACKRecv"
    -- , "TCPDSACKUndo"
    -- , "TCPDeferAcceptDrop"
    -- , "TCPDirectCopyFromBacklog"
    -- , "TCPDirectCopyFromPrequeue"
    -- , "TCPFACKReorder"
    -- , "TCPFastOpenActive"
    -- , "TCPFastOpenActiveFail"
    -- , "TCPFastOpenCookieReqd"
    -- , "TCPFastOpenListenOverflow"
    -- , "TCPFastOpenPassive"
    -- , "TCPFastOpenPassiveFail"
  , "TCPFastRetrans"
  , "TCPForwardRetrans"
    -- , "TCPFromZeroWindowAdv"
    -- , "TCPFullUndo"
    -- , "TCPHPAcks"
    -- , "TCPHPHits"
    -- , "TCPHPHitsToUser"
    -- , "TCPHystartDelayCwnd"
    -- , "TCPHystartDelayDetect"
    -- , "TCPHystartTrainCwnd"
    -- , "TCPHystartTrainDetect"
  , "TCPKeepAlive"
  , "TCPLossFailures"
  , "TCPLossProbeRecovery"
  , "TCPLossProbes"
  , "TCPLossUndo"
  , "TCPLostRetransmit"
    -- , "TCPMD5Failure"
    -- , "TCPMD5NotFound"
    -- , "TCPMD5Unexpected"
    -- , "TCPMTUPFail"
    -- , "TCPMTUPSuccess"
  , "TCPMemoryPressures"
    -- , "TCPMinTTLDrop"
    -- , "TCPOFODrop"
    -- , "TCPOFOMerge"
    -- , "TCPOFOQueue"
    -- , "TCPOrigDataSent"
    -- , "TCPPartialUndo"
    -- , "TCPPrequeueDropped"
    -- , "TCPPrequeued"
    -- , "TCPPureAcks"
    -- , "TCPRcvCoalesce"
  , "TCPRcvCollapsed"
    -- , "TCPRenoFailures"
    -- , "TCPRenoRecovery"
    -- , "TCPRenoRecoveryFail"
    -- , "TCPRenoReorder"
    -- , "TCPReqQFullDoCookies"
    -- , "TCPReqQFullDrop"
  , "TCPRetransFail"
    -- , "TCPSACKDiscard"
    -- , "TCPSACKReneging"
    -- , "TCPSACKReorder"
    -- , "TCPSYNChallenge"
    -- , "TCPSackFailures"
    -- , "TCPSackMerged"
    -- , "TCPSackRecovery"
    -- , "TCPSackRecoveryFail"
    -- , "TCPSackShiftFallback"
    -- , "TCPSackShifted"
    -- , "TCPSchedulerFailed"
  , "TCPSlowStartRetrans"
    -- , "TCPSpuriousRTOs"
    -- , "TCPSpuriousRtxHostQueues"
  , "TCPSynRetrans"
    -- , "TCPTSReorder"
    -- , "TCPTimeWaitOverflow"
  , "TCPTimeouts"
    -- , "TCPToZeroWindowAdv"
    -- , "TCPWantZeroWindowAdv"
    -- , "TCPWinProbe"
    -- , "TW"
    -- , "TWKilled"
    -- , "TWRecycled"

  ]) ++ (nsUdpCounterMetric <$> [
    "IgnoredMulti"
  , "InCsumErrors"
  , "InDatagrams"
  , "InErrors"
  , "NoPorts"
  , "OutDatagrams"
  , "RcvbufErrors"
  , "SndbufErrors"
  ])


type NSMetricBuilder = Text -> Maybe MetricDatum

netStatMetricsBuilders :: Args
                       -> NetStatLookup
                       -> (NSMetricBuilder, NSMetricBuilder, NSMetricBuilder)
netStatMetricsBuilders Args{..} NetStatLookup{..} = (nsIpCounterMetric, nsTcpCounterMetric, nsUdpCounterMetric)

  where nsIpCounterMetric  = (mfilter (const argsNetIP))
                             . (netStatMetric "Ip" netStatLookupIp)

        nsTcpCounterMetric = (mfilter (const argsNetTCP))
                             . (netStatMetric "Tcp" netStatLookupTcp)

        nsUdpCounterMetric = (mfilter (const argsNetUDP))
                             . (netStatMetric "Udp" netStatLookupUdp)

        netStatMetric prefix lookupField fieldName =
            nsDatum ("NetStat" <> prefix <> fieldName) <$> (lookupField fieldName >>= nonZero)

        nsDatum n v = metricDatum n & mdValue .~ (Just . fromIntegral) v
                                    & mdUnit .~ Just Count

        nonZero x | x == 0    = Nothing
                  | otherwise = Just x
