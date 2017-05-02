{-# LANGUAGE OverloadedStrings  #-}


module Loadavg (
  loadavg
, parseLoadavg
) where


import           Control.Monad.Catch (MonadCatch)
import           Control.Monad.Logger
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Attoparsec.Text as AT
import           Parse
import           Types


loadavg :: (MonadCatch m, MonadResource m, MonadLogger m)
        => m (Either String Loadavg)
loadavg = parseFile parseLoadavg "/proc/loadavg"


-- Parses the linux's /proc/loadavg entries
parseLoadavg :: Parser Loadavg
parseLoadavg = do
  cpu1 <- (double <* skipSpace) <?> "CPU1"
  cpu5 <- (double <* skipSpace) <?> "CPU5"
  cpu10 <- (double <* skipSpace) <?> "CPU10"
  pRunning <- (decimal <* char '/') <?> "ProcRunning"
  pTotal <- decimal <?> "ProcTotal"

  return $ Loadavg cpu1 cpu5 cpu10 pRunning pTotal
