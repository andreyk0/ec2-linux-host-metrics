{-# LANGUAGE OverloadedStrings #-}

module Args (
  Args(..)
, ArgMountPoint
, parseCliArgs
) where

import           Data.Either
import           Data.Monoid
import qualified Data.Text as T
import           Network.AWS.CloudWatch.Types
import           Options.Applicative
import qualified Text.PrettyPrint.ANSI.Leijen as PP

type ArgMountPoint = T.Text

data Args = Args { argsVerbose :: !Bool
                 , argsVersion :: !Bool
                 , argsPublishMetrics :: !Bool
                 , argsDimensions :: ![[Dimension]]
                 , argsCPU :: !Bool
                 , argsMemory :: !Bool
                 , argsDisk:: !(Maybe [ArgMountPoint])
                 , argsNTP:: !Bool
                 , argsNetIP:: !Bool
                 , argsNetTCP:: !Bool
                 , argsNetUDP:: !Bool
                 } deriving (Show)


parseArgs :: Parser Args
parseArgs =
  Args <$> switch
           ( long "verbose"
          <> short 'v'
          <> help "Be verbose.")
       <*> switch
           ( long "version"
          <> short 'V'
          <> help "Print version and exit.")
       <*> switch
           ( long "publish-metrics"
          <> short 'p'
          <> help "Publish generated metrics to CloudWatch.")
       <*> many ( option parseDimensions
                  ( long "metric-dimensions"
                 <> short 'd'
                 <> metavar "name=val[,name1=val1]"
                 <> help "Dimension(s) to attach to all generated data. A special value INSTANCE_ID will be replaced with an actual current instance ID." )
                )
       <*> switch
           ( long "cpu"
          <> help "Generate CPU metrics.")
       <*> switch
           ( long "memory"
          <> help "Generate memory metrics.")
       <*> optional (option parseMountPoints
                      ( long "disk"
                    <> metavar "/,/var,/data,..."
                    <> help "Generate disk metrics for given mount points (or all if none specified, e.g. --disk=" ))
       <*> switch
           ( long "ntp"
          <> help "Generate NTP drift metrics.")
       <*> switch
           ( long "net-ip"
          <> help "Generate network IP metrics.")
       <*> switch
           ( long "net-tcp"
          <> help "Generate network TCP metrics.")
       <*> switch
           ( long "net-udp"
          <> help "Generate network UDP metrics.")

  where -- each dimension argument consists of one or more comma-separated name=val pairs
        -- e..g foo=bar,baz=blah
        parseDimensions = eitherReader $ \s ->
          let err = Left $ "Failed to parse metric dimension " <> s <> ", expected dimension in the name=value format."

              parseDim txt = case T.split (== '=') txt
                               of [n, v] -> Right $ dimension n v
                                  _      -> err

          in case T.split (== ',') (T.pack s)
             of [""] -> err
                []   -> err
                txt -> let (es,ds) = partitionEithers $ parseDim <$> txt
                        in if null es then Right ds else err

        parseMountPoints = eitherReader $ \s ->
          Right <$> filter (not . T.null) $ T.split (== ',') (T.pack s)


parseCliArgs :: IO Args
parseCliArgs = execParser opts
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> header "Publishes a few metrics about linux hosts to CloudWatch."
     <> footerDoc (Just progDescDoc'))

    progDescDoc' =
      PP.text "Source: https://github.com/gilt/ec2-linux-host-metrics"
      PP.<+> PP.linebreak
      PP.<$> PP.text "At least one set of dimensions is required. A special dimension value INSTANCE_ID will be replaced with an actual current instance ID."
      PP.<+> PP.linebreak
      PP.<$> PP.text "Metrics will be published once per each given set of dimensions to support different levels of grouping."
      PP.<+> PP.linebreak
      PP.<$> PP.text "E.g. command:"
      PP.<+> PP.linebreak
      PP.<$> PP.text "\tec2-linux-host-metrics --publish-metrics --metric-dimensions my-app=test --metric-dimensions my-app=test,InstanceId=INSTANCE_ID"
      PP.<+> PP.linebreak
      PP.<$> PP.text "If none of the more specific options are given - all metrics are generated/published."
