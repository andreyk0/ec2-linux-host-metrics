{-# LANGUAGE OverloadedStrings  #-}


module Stat (
  statsSinceLastRun
, parseStat
) where


import           Control.Monad (join)
import           Control.Monad.Catch (MonadCatch)
import qualified Control.Monad.Catch as MC
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text as AT
import           Data.Bifunctor
import           Data.Monoid
import           Parse
import           System.Directory
import           Types


-- Reads /proc/stat and last saved version of it.
-- Saves current version of /proc/stat for the next run.
--
-- /proc/stat fields are for the most part ever-incrementing counters
-- running since boot time, need to diff them to get the CPU metrics
-- relative to last run.
statsSinceLastRun :: (MonadCatch m, MonadResource m, MonadLogger m, MonadIO m)
                  => m (Either String (Maybe Stat, Stat))
statsSinceLastRun = do
  res <- MC.try tryToParse
  fmap join $ return $ first (\ (MC.SomeException e) ->  "Unable to proc stat " <> show e) res

  where tryToParse = do
          let procStatFile = "/proc/stat"
              tmpStatFile = "/tmp/ec2-linux-host-metrics-proc-stat"

          hasPreviousStatFile <- liftIO $ doesFileExist tmpStatFile

          previousState <- if hasPreviousStatFile
                           then fmap Just <$> parseStatFile tmpStatFile
                           else return $ Right Nothing

          currentState <- parseStatFile procStatFile
          liftIO $ copyFile procStatFile tmpStatFile -- save current version for the next run

          return $ do prevS <- previousState
                      curS <- currentState
                      return (prevS, curS)


parseStatFile :: (MonadCatch m, MonadResource m, MonadLogger m)
              => FilePath
              -> m (Either String Stat)
parseStatFile = parseFile parseStat


-- Parses the linux's /proc/loadavg entries
parseStat :: Parser Stat
parseStat = Stat
  <$> ( (string "cpu" >> skipSpace >> decimal) <?> "CPUUser")
  <*> ( (skipSpace >> decimal) <?> "CPUNice" )
  <*> ( (skipSpace >> decimal) <?> "CPUSystem" )
  <*> ( (skipSpace >> decimal) <?> "CPUIdle" )
  <*> ( (skipSpace >> decimal) <?> "CPUIOWait" )
  <*> ( (skipSpace >> decimal) <?> "CPUIOIRQ" )
  <*> ( (skipSpace >> decimal) <?> "CPUSoftIRQ" )
  <*> ( (skipSpace >> decimal) <?> "CPUSteal" )
  <*> ( (skipSpace >> decimal) <?> "CPUGuest" )
  <*> ( (skipSpace >> decimal) <?> "CPUNice" )
  <*> nextField "intr" "Intr"
  <*> nextField "ctxt" "Ctxt"
  <*> nextField "processes" "Procs"

  where nextField fName fHelp = ( manyTill anyChar (try $ string fName) >>
                                  skipSpace >>
                                  decimal
                                ) <?> fHelp
