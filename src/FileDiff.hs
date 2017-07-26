{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}


module FileDiff (
  fileDiff
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


-- | Maintains a saved copy of a given /pro/some/thing file in /tmp,
-- parses both with a given parser.
-- Can be used to compute diffs between individual runs of the
-- monotonic counter fields.
--
-- /proc/stat fields are for the most part ever-incrementing counters
-- running since boot time, need to diff them to get the CPU metrics
-- relative to last run.
fileDiff :: (MonadCatch m, MonadResource m, MonadLogger m, MonadIO m, Show a)
         => FilePath -- ^ /proc/file/path
         -> Parser a -- ^ parser for this file
         -> m (Either String (Maybe a, a)) -- ^ old (if any) and a current parsed value
fileDiff procFile fileParser = do
  res <- MC.try tryToParse
  fmap join $ return $ first (\ (MC.SomeException e) ->  "Unable to process " <> procFile <> " diffs: " <> show e) res

  where tryToParse = do
          let tmpFile = "/tmp/ec2-linux-host-metrics-" <> fmap (\case '/' -> '-' ; x -> x) procFile

          hasPreviousVersion <- liftIO $ doesFileExist tmpFile

          previous <- if hasPreviousVersion
                      then fmap Just <$> parseFile fileParser tmpFile
                      else return $ Right Nothing

          liftIO $ copyFile procFile tmpFile -- save current version for the next run
          current <- parseFile fileParser tmpFile -- parse same snapshot of data

          return $ (,) <$> previous <*> current
