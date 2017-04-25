{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Main where

import App
import Args
import Control.Lens
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Trans.AWS
import Data.List.Split
import Df
import Network.AWS.CloudWatch
import Types


main:: IO ()
main = runApp $ do
  args@Args{..} <- getArgs
  $(logDebugSH) args

  myInstanceID <- getInstanceID
  $(logDebugSH) myInstanceID

  df <- diskFree
  $(logDebugSH) df

  let dfMetrics = concat $ df <&> dfFsMetricData
      extraDimensions = dimension "InstanceId" myInstanceID : argsExtraDimensions -- added to all generated metrics
      allMetrics = dfMetrics <&> mdDimensions %~ (++ extraDimensions)


  if argsPublishMetrics
    then do $(logDebugSH) allMetrics
            forM_ (chunksOf 20 allMetrics) $ \ms -> -- split into chunks to avoid hitting the call size limit
                void . send $ putMetricData "System/Linux" & pmdMetricData .~ ms

    else forM_ allMetrics $(logInfoSH)


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
                           else (fromIntegral dffsCapacity)
                  in 100 * (fromIntegral dffsUsed) / cap
