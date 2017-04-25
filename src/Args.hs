module Args (
  Args(..)
, parseCliArgs
) where


import           Data.Monoid
import qualified Data.Text as T
import           Network.AWS.CloudWatch.Types
import           Options.Applicative


data Args = Args { argsVerbose :: !Bool
                 , argsVersion :: !Bool
                 , argsPublishMetrics :: !Bool
                 , argsExtraDimensions :: ![Dimension]
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
       <*> many ( option parseDimension
                  ( long "metric-dimension"
                 <> short 'd'
                 <> metavar "name=value"
                 <> help "Extra dimension to attach to all generated data" )
                )

  where parseDimension = eitherReader $ \s ->
          case T.split (== '=') (T.pack s)
            of n : v : [] -> Right $ dimension n v
               _          -> Left $ "Failed to parse custom argument " <> s <> ", expected extra dimension in the name=value format"



parseCliArgs :: IO Args
parseCliArgs = execParser opts
  where
    opts = info (helper <*> parseArgs)
      ( fullDesc
     <> header "Publishes a few metrics about linux hosts to CloudWatch."
     <> footer "Source: https://github.com/gilt/ec2-linux-host-metrics")
