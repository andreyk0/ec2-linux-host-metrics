{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Main where

import           App
import           Args
import           Control.Lens
import           Control.Monad
import           Control.Monad.Logger
import           Control.Monad.Trans.AWS
import           Data.Either
import           Data.List.Split
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import           Df
import           Meminfo
import           Network.AWS.CloudWatch
import           Ntp
import           Types


main:: IO ()
main = runApp $ do
  args@Args{..} <- getArgs
  $(logDebugSH) args

  myInstanceID <- getInstanceID
  $(logDebugSH) myInstanceID

  dfMetrics <- diskFree <&> fmap dfMetricData

  memMetrics <- meminfo <&> fmap memMetricData

  ntpMetrics <- ntpQuery <&> fmap ntpMetricData

  allDimensionlessMetrics <- collateResults [ dfMetrics, memMetrics, ntpMetrics ]

  let extraDimensions = dimension "InstanceId" myInstanceID : argsExtraDimensions -- added to all generated metrics
      allMetrics = allDimensionlessMetrics <&> mdDimensions %~ (<> extraDimensions)

  if argsPublishMetrics
    then do $(logDebugSH) allMetrics
            forM_ (chunksOf 20 allMetrics) $ \ms -> -- split into chunks to avoid hitting the call size limit
                void . send $ putMetricData "System/Linux" & pmdMetricData .~ ms

    else forM_ allMetrics $(logInfoSH)


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

      memUsed = do t <- mieVal <$> memTotal
                   f <- mieVal <$> memFree
                   b <- mieVal <$> buffers
                   c <- mieVal <$> cached

                   return $ MeminfoEntry "MemUsed" ( t - f - b - c ) -- MemTotal - (MemFree + Buffers + Cached)

      memUsedPercent = do u <- mieVal <$> memUsed
                          t <- mieVal <$> memTotal
                          let t' = if t == 0 then 1 else t
                          return $ 100 * fromIntegral u / fromIntegral t'


   in catMaybes [ -- report whatever entries we can find
        memEntryBytesDatum <$> memTotal
      , memEntryBytesDatum <$> memFree
      , memEntryBytesDatum <$> memAvailable
      , memEntryBytesDatum <$> memUsed
      , memPercentDatum "MemUsedPercent" <$> memUsedPercent
      ]

  where memEntryBytesDatum MeminfoEntry{..} =
          metricDatum mieKey & mdValue .~ (Just . fromIntegral) mieVal
                             & mdUnit .~ Just Bytes

        memPercentDatum k v =
          metricDatum k & mdValue .~ Just v
                        & mdUnit .~ Just Percent


ntpMetricData :: Ntp
              -> [MetricDatum]
ntpMetricData Ntp{..} = [
    metricDatum "NtpOffset" & mdValue .~ Just ntpOffset
                            & mdUnit .~ Just Milliseconds

  , metricDatum "NtpJitter" & mdValue .~ Just ntpJitter
                            & mdUnit .~ Just Milliseconds
  ]
