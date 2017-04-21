module Args (
  Args(..)
, parseCliArgs
) where


import           Data.Monoid
import           Options.Applicative


data Args = Args { argsVerbose :: !Bool
                 , argsVersion :: !Bool
                 , argsPublishMetrics :: !Bool
                 } deriving (Show)


parseArgs :: Parser Args
parseArgs = Args
     <$> switch
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


parseCliArgs :: IO Args
parseCliArgs = execParser opts
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> header "Publishes a few metrics about linux hosts to CloudWatch."
     <> footer "Source: https://github.com/gilt/ec2-linux-host-metrics")
