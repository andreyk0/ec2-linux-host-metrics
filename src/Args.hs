{-# LANGUAGE OverloadedStrings #-}

module Args (
  Args(..)
, parseCliArgs
) where


import           Data.Either
import           Data.Monoid
import qualified Data.Text as T
import           Network.AWS.CloudWatch.Types
import           Options.Applicative
import qualified Text.PrettyPrint.ANSI.Leijen as PP


data Args = Args { argsVerbose :: !Bool
                 , argsVersion :: !Bool
                 , argsPublishMetrics :: !Bool
                 , argsExtraDimensions :: ![[Dimension]]
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
                 <> help "Extra dimension(s) to attach to all generated data." )
                )

  where -- each dimension argument consists of one or more comma-separated name=val pairs
        -- e..g foo=bar,baz=blah
        parseDimensions = eitherReader $ \s ->
          let err = Left $ "Failed to parse metric dimension " <> s <> ", expected extra dimension in the name=value format"

              parseDim txt = case T.split (== '=') txt
                               of [n, v] -> Right $ dimension n v
                                  _      -> err

          in case T.split (== ',') (T.pack s)
             of [""] -> err
                []   -> err
                txt -> let (es,ds) = partitionEithers $ parseDim <$> txt
                        in if null es then Right ds else err


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
      PP.<$> PP.text "Each set of metrics will be published with InstanceId dimension."
      PP.<+> PP.linebreak
      PP.<$> PP.text "In addition it'll be published once per each --metric-dimensions option value with dimensions specified by the option."
